/**
 * R Workbench: acquisition, cleaning, value gating, statistical analysis, and
 * reporting through a local R toolchain.
 *
 * The plugin registers six tools and shells out to standalone R stage scripts.
 * Every run is a self-contained directory that can be opened as an RStudio
 * project; the value gate is enforced by the R stages, not only by prose.
 */
import { spawn } from 'node:child_process'
import { promises as fs } from 'node:fs'
import { existsSync } from 'node:fs'
import path from 'node:path'
import os from 'node:os'
import { fileURLToPath } from 'node:url'

export const name = 'r-workbench'
export const inject = ['tools', 'workingDirectory']

const PLUGIN_VERSION = '1.0.0'
const PLUGIN_ROOT = path.dirname(fileURLToPath(import.meta.url))
const R_DIR = path.join(PLUGIN_ROOT, 'r')
const SUPPORTED_EXTENSIONS = new Set([
  'csv', 'tsv', 'tab', 'txt', 'dat', 'psv', 'text', 'log',
  'json', 'jsonl', 'ndjson', 'xlsx', 'xlsm', 'xls', 'et', 'ett', 'ods', 'zip',
])

/** Validate the JSON-schema subset this plugin declares (type/properties/required/items/enum). */
function validateSchema(schema, value, at = 'arguments') {
  const errors = []
  if (!schema || typeof schema !== 'object') return errors
  if (Array.isArray(schema.enum) && !schema.enum.some(candidate => Object.is(candidate, value))) {
    errors.push(`${at} must be one of ${schema.enum.map(v => JSON.stringify(v)).join(', ')}`)
    return errors
  }
  if (schema.const !== undefined && !Object.is(schema.const, value)) {
    errors.push(`${at} must equal ${JSON.stringify(schema.const)}`)
    return errors
  }
  switch (schema.type) {
    case 'object': {
      if (value === null || typeof value !== 'object' || Array.isArray(value)) {
        errors.push(`${at} must be an object`)
        return errors
      }
      const properties = schema.properties ?? {}
      for (const key of schema.required ?? []) {
        if (value[key] === undefined) errors.push(`${at}.${key} is required`)
      }
      for (const [key, child] of Object.entries(properties)) {
        if (value[key] !== undefined) errors.push(...validateSchema(child, value[key], `${at}.${key}`))
      }
      if (schema.additionalProperties === false) {
        for (const key of Object.keys(value)) {
          if (!(key in properties)) errors.push(`${at}.${key} is not allowed`)
        }
      }
      break
    }
    case 'array': {
      if (!Array.isArray(value)) {
        errors.push(`${at} must be an array`)
        return errors
      }
      if (schema.items !== undefined) {
        value.forEach((item, index) => errors.push(...validateSchema(schema.items, item, `${at}[${index}]`)))
      }
      break
    }
    case 'string':
      if (typeof value !== 'string') errors.push(`${at} must be a string`)
      break
    case 'number':
      if (typeof value !== 'number' || !Number.isFinite(value)) errors.push(`${at} must be a finite number`)
      break
    case 'integer':
      if (typeof value !== 'number' || !Number.isInteger(value)) errors.push(`${at} must be an integer`)
      break
    case 'boolean':
      if (typeof value !== 'boolean') errors.push(`${at} must be a boolean`)
      break
    case 'null':
      if (value !== null) errors.push(`${at} must be null`)
      break
    default:
      break
  }
  return errors
}

/** Compile the authoring property map into the object-rooted JSON Schema the registry expects. */
function compileParameters(spec) {
  if (spec && spec.type === 'object' && spec.properties !== undefined) return spec
  const properties = {}
  const required = []
  for (const [key, value] of Object.entries(spec ?? {})) {
    const { required: isRequired, ...schema } = value
    properties[key] = schema
    if (isRequired === true) required.push(key)
  }
  return { type: 'object', properties, required, additionalProperties: false }
}

/** One tool definition with argument validation and a text-only canonical result projection. */
function defineTool(definition) {
  const run = definition.execute
  const parameters = compileParameters(definition.parameters)
  return {
    ...definition,
    parameters,
    async execute(args, exec) {
      const errors = validateSchema(parameters, args)
      if (errors.length > 0) throw new Error(`r-workbench: invalid arguments: ${errors.join('; ')}`)
      return run(args, exec)
    },
    output: {
      schema: { type: 'object' },
      render(_args, value) {
        const text = value && typeof value.summary === 'string'
          ? value.summary
          : JSON.stringify(value)
        return [{ type: 'text', text }]
      },
    },
  }
}

/** Resolve `~`, then a workspace-relative path. */
function expandPath(input, cwd) {
  let value = String(input)
  if (value === '~') value = os.homedir()
  else if (value.startsWith('~/')) value = path.join(os.homedir(), value.slice(2))
  return path.isAbsolute(value) ? value : path.resolve(cwd, value)
}

async function listDataFiles(dir) {
  const entries = await fs.readdir(dir, { withFileTypes: true })
  return entries
    .filter(entry => entry.isFile())
    .map(entry => entry.name)
    .filter(name => SUPPORTED_EXTENSIONS.has(path.extname(name).slice(1).toLowerCase()))
    .sort()
    .map(name => path.join(dir, name))
}

function timestamp() {
  const d = new Date()
  const pad = n => String(n).padStart(2, '0')
  return `${d.getFullYear()}${pad(d.getMonth() + 1)}${pad(d.getDate())}-${pad(d.getHours())}${pad(d.getMinutes())}${pad(d.getSeconds())}`
}

function slugify(value) {
  const base = path.basename(value, path.extname(value))
  const slug = base
    .normalize('NFKC')
    .replace(/[^\p{L}\p{N}]+/gu, '-')
    .replace(/^-+|-+$/g, '')
    .slice(0, 36)
  return slug.length > 0 ? slug : 'dataset'
}

function asArray(value) {
  if (value === null || value === undefined) return []
  return Array.isArray(value) ? value : [value]
}

function tail(text, limit = 8000) {
  if (text.length <= limit) return text
  return `…（截断）\n${text.slice(-limit)}`
}

async function readJson(pathname) {
  return JSON.parse(await fs.readFile(pathname, 'utf8'))
}

/** Build a summary line block for one analysis. */
function analysisLine(analysis, index) {
  if (analysis.error) return `- 分析 ${index} 失败：${analysis.error}`
  const p = analysis.pAdjusted
  const pText = p === null || p === undefined ? '—'
    : p < 0.001 ? '<0.001' : Number(p).toFixed(3)
  const nText = analysis.type === 'trend' && analysis.months
    ? `${analysis.months} 个月度均值（基于 ${analysis.n} 条观测）`
    : `n=${analysis.n}`
  return `- ${analysis.title}（${nText}，校正后 p=${pText}）`
}

export function apply(ctx, config = {}) {
  const runsDirName = config.runsDirName ?? 'r-workbench-runs'
  if (!/^[A-Za-z0-9._-]+$/.test(runsDirName)) {
    throw new Error('r-workbench: runsDirName must be one directory name (letters, digits, ".", "_", "-")')
  }
  const rscript = config.rscriptPath ?? 'Rscript'
  const timeouts = {
    env: 60_000,
    ingest: 600_000,
    assess: 300_000,
    analyze: 900_000,
    report: 600_000,
    run: 300_000,
    ...(config.timeouts ?? {}),
  }

  async function workspaceOf(exec) {
    if (!exec.agent) throw new Error('r-workbench requires an owning agent session')
    return ctx.workingDirectory.ensure(exec.agent, exec.signal)
  }

  async function runsRoot(exec) {
    const cwd = await workspaceOf(exec)
    return path.join(cwd, runsDirName)
  }

  async function runR(stage, runDir, payload, exec, timeoutMs) {
    const logsDir = path.join(runDir, 'logs')
    await fs.mkdir(logsDir, { recursive: true })
    const configPath = path.join(logsDir, `${stage}-config.json`)
    const resultPath = path.join(logsDir, `${stage}-result.json`)
    await fs.writeFile(configPath, `${JSON.stringify(payload, null, 2)}\n`)
    await fs.rm(resultPath, { force: true })
    const script = path.join(R_DIR, `${stage}.R`)
    if (!existsSync(script)) throw new Error(`r-workbench: missing R stage script ${script}`)

    const child = spawn(rscript, ['--vanilla', script, configPath], {
      cwd: runDir,
      env: { ...process.env },
      stdio: ['ignore', 'pipe', 'pipe'],
    })
    let stdout = ''
    let stderr = ''
    let timedOut = false
    let aborted = false
    child.stdout.on('data', chunk => { stdout += chunk })
    child.stderr.on('data', chunk => { stderr += chunk })
    const timer = setTimeout(() => {
      timedOut = true
      child.kill('SIGTERM')
      setTimeout(() => child.kill('SIGKILL'), 3000).unref()
    }, timeoutMs)
    const onAbort = () => {
      aborted = true
      child.kill('SIGTERM')
    }
    exec.signal?.addEventListener('abort', onAbort, { once: true })
    let exitCode
    try {
      exitCode = await new Promise((resolve, reject) => {
        child.once('error', reject)
        child.once('close', resolve)
      })
    } finally {
      clearTimeout(timer)
      exec.signal?.removeEventListener('abort', onAbort)
    }
    if (aborted) throw new Error('r-workbench: execution was cancelled')
    if (timedOut) throw new Error(`r-workbench: R stage "${stage}" exceeded ${Math.round(timeoutMs / 1000)}s`)

    let result
    try {
      result = await readJson(resultPath)
    } catch {
      const detail = stderr.trim() || stdout.trim() || `Rscript exited with code ${exitCode}`
      throw new Error(`r-workbench: R stage "${stage}" produced no result. ${tail(detail)}`)
    }
    if (!result.ok) {
      throw new Error(`r-workbench: R stage "${stage}" failed: ${result.error ?? 'unknown error'}`)
    }
    return result.data
  }

  async function resolveRun(exec, ref) {
    const cwd = await workspaceOf(exec)
    const root = path.join(cwd, runsDirName)
    const rootReal = await fs.realpath(root).catch(() => path.resolve(root))
    let candidate
    if (ref) {
      if (path.isAbsolute(ref)) candidate = ref
      else if (ref.includes('/') || ref.includes('\\')) candidate = path.resolve(cwd, ref)
      else candidate = path.join(root, ref)
      const real = await fs.realpath(candidate).catch(() => null)
      if (!real) throw new Error(`r-workbench: run not found: ${ref}`)
      if (real !== rootReal && !real.startsWith(`${rootReal}${path.sep}`)) {
        throw new Error(`r-workbench: run must live under ${rootReal}`)
      }
      if (!existsSync(path.join(real, 'run.json'))) {
        throw new Error(`r-workbench: ${real} is not a completed run (missing run.json)`)
      }
      return { dir: real, meta: await readJson(path.join(real, 'run.json')) }
    }
    const entries = await fs.readdir(root, { withFileTypes: true }).catch(() => [])
    const candidates = []
    for (const entry of entries) {
      if (!entry.isDirectory()) continue
      const dir = path.join(root, entry.name)
      if (!existsSync(path.join(dir, 'run.json'))) continue
      const stat = await fs.stat(path.join(dir, 'run.json'))
      candidates.push({ dir, mtime: stat.mtimeMs })
    }
    candidates.sort((a, b) => b.mtime - a.mtime)
    if (candidates.length === 0) throw new Error('r-workbench: no runs yet; call r_ingest first')
    const dir = candidates[0].dir
    return { dir, meta: await readJson(path.join(dir, 'run.json')) }
  }

  async function createRun(exec, paths, requestedId, title) {
    const cwd = await workspaceOf(exec)
    const root = path.join(cwd, runsDirName)
    await fs.mkdir(root, { recursive: true })
    const baseId = requestedId
      ? String(requestedId)
      : `${slugify(paths[0])}-${timestamp()}`
    if (!/^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$/.test(baseId)) {
      throw new Error('r-workbench: runId must start with a letter or digit and contain only letters, digits, ".", "_", "-"')
    }
    let runId = baseId
    let dir = path.join(root, runId)
    for (let suffix = 2; suffix < 100; suffix += 1) {
      try {
        await fs.mkdir(dir, { recursive: false })
        break
      } catch (error) {
        if (error.code !== 'EEXIST') throw error
        if (!requestedId) {
          runId = `${baseId}-${suffix}`
          dir = path.join(root, runId)
          continue
        }
        throw new Error(`r-workbench: run "${baseId}" already exists`)
      }
    }
    await fs.mkdir(path.join(dir, 'data'), { recursive: true })
    await fs.mkdir(path.join(dir, 'outputs'), { recursive: true })
    await fs.mkdir(path.join(dir, 'logs'), { recursive: true })
    await fs.mkdir(path.join(dir, 'scripts'), { recursive: true })
    const proj = [
      'Version: 1.0',
      '',
      'RestoreWorkspace: No',
      'SaveWorkspace: No',
      'AlwaysSaveHistory: No',
      '',
      'EnableCodeIndexing: Yes',
      'UseSpacesForTab: Yes',
      'NumSpacesForTab: 2',
      'Encoding: UTF-8',
      '',
      'RnwWeave: knitr',
      'LaTeX: XeLaTeX',
      '',
    ].join('\n')
    await fs.writeFile(path.join(dir, `${runId}.Rproj`), proj)
    await fs.writeFile(path.join(dir, 'scripts', 'explore.R'), [
      '# 在当前运行目录中探索清洗后的数据。',
      'clean <- readRDS("data/clean.rds")',
      'str(clean)',
      'summary(clean)',
      '',
      '# 在此写你的 R 代码；也可以调用插件提供的统计分析结果。',
      '',
    ].join('\n'))
    return { runId, dir, title: title ?? path.basename(paths[0], path.extname(paths[0])) }
  }

  function payloadFor(runDir, runId, extra = {}) {
    return {
      runDir,
      runId,
      pluginVersion: PLUGIN_VERSION,
      pandocPath: config.pandocPath ?? '',
      ...extra,
    }
  }

  ctx.effect(() => ctx.tools.register(defineTool({
    name: 'r_env',
    description: '检查本机 R 工具链：R、RStudio、Quarto、pandoc、必需与可选 R 包、中文字体、磁盘空间，并列出最近的运行。采集或分析前先调用。返回 ready=false 时按 installCommand 安装依赖。',
    parameters: { type: 'object', properties: {}, additionalProperties: false },
    timeoutMs: timeouts.env,
    async execute(_args, exec) {
      const root = await runsRoot(exec)
      const runDir = path.join(root, '.env-check')
      await fs.mkdir(path.join(runDir, 'logs'), { recursive: true })
      const data = await runR('env', runDir, payloadFor(runDir, '_env', {
        runsDir: root,
        pandocPath: config.pandocPath ?? '',
      }), exec, timeouts.env)
      const packages = asArray(data.packages)
      const missing = asArray(data.missingRequired)
      const recent = asArray(data.recentRuns).slice(0, 5)
      const lines = [
        `R ${String(data.rVersion).replace(/^R version /, '')} · RStudio ${data.rstudio ? '已安装' : '未安装'} · Quarto ${data.quarto ? '已安装' : '未安装'} · pandoc ${data.pandoc ? '已安装' : '未找到'}`,
        `必需包 ${packages.filter(p => p.role === 'required' && p.installed).length}/${packages.filter(p => p.role === 'required').length} 就绪；可选包 ${packages.filter(p => p.role === 'optional' && p.installed).length}/${packages.filter(p => p.role === 'optional').length}`,
        `中文字体：${data.cjkFont ?? '未检测到'} · 运行根目录：${data.runsDir}`,
        data.ready ? '状态：READY' : `状态：MISSING ${missing.join(', ')}`,
      ]
      if (!data.ready) lines.push(`安装命令：${data.installCommand}`)
      if (recent.length > 0) {
        lines.push('最近运行：')
        for (const run of recent) {
          lines.push(`- ${run.runId} · ${run.rows ?? '?'} 行 × ${run.cols ?? '?'} 列 · ${run.decision ?? '未评估'}${run.score ? `（${Number(run.score).toFixed(1)}）` : ''}`)
        }
      }
      const warnings = asArray(data.warnings)
      if (warnings.length) lines.push(...warnings.map(w => `警告：${w}`))
      return { summary: lines.join('\n'), data }
    },
    presentCall: args => ({ card: 'generic', title: 'Check R toolchain', kind: 'read', rawInput: args }),
  })))

  ctx.effect(() => ctx.tools.register(defineTool({
    name: 'r_ingest',
    description: '用 R 采集并清洗数据：CSV/TSV/文本、Excel/WPS（xlsx/xls/et）、ODS、JSON/NDJSON、ZIP。自动识别编码（UTF-8/GB18030）、分隔符、表头与类型，处理缺失标记、重复行和常量列。paths 可以是文件或目录（目录取一层内支持的文件），多个文件按列名合并并增加 source_file 列。返回 run 标识、行/列数、列字典和警告；随后必须调用 r_assess 做价值评估。',
    parameters: {
      paths: {
        type: 'array',
        required: true,
        items: { type: 'string' },
        description: '文件路径或目录；相对路径按当前工作目录解析，支持 ~。',
      },
      title: { type: 'string', description: '报告标题；默认取第一个文件名。' },
      runId: { type: 'string', description: '可选的运行标识；默认“文件名-时间戳”。' },
      sheet: { type: 'string', description: 'Excel/ODS 工作表名或序号；"*" 表示读取全部工作表。默认第一个工作表。' },
      format: { type: 'string', enum: ['auto', 'delimited', 'excel', 'json', 'zip', 'ods'], description: '强制文件类型；默认 auto 按扩展名与文件头判断。' },
      delimiter: { type: 'string', description: '强制分隔符（如 ","、";"、tab）；默认自动嗅探。' },
      header: { type: 'boolean', description: '首行是否为表头；默认自动检测。' },
      skip: { type: 'number', description: '跳过文件开头若干行。' },
      dedupe: { type: 'boolean', description: '是否移除完全重复行；默认 true。' },
      dropConstant: { type: 'boolean', description: '是否移除常量列；默认 true。' },
      maxRows: { type: 'number', description: '最多读取的数据行数（Excel）。' },
      maxFileBytes: { type: 'number', description: '单文件大小上限，默认 512 MiB。' },
    },
    timeoutMs: timeouts.ingest,
    async execute(args, exec) {
      const cwd = await workspaceOf(exec)
      const expanded = []
      for (const input of args.paths ?? []) {
        const resolved = expandPath(input, cwd)
        const stat = await fs.stat(resolved).catch(() => null)
        if (!stat) throw new Error(`r-workbench: path not found: ${input}`)
        if (stat.isDirectory()) {
          const files = await listDataFiles(resolved)
          if (files.length === 0) throw new Error(`r-workbench: no supported data files in ${resolved}`)
          expanded.push(...files.slice(0, 50))
        } else {
          expanded.push(resolved)
        }
      }
      const paths = [...new Set(expanded)]
      if (paths.length === 0) throw new Error('r-workbench: paths resolved to no files')
      const run = await createRun(exec, paths, args.runId, args.title)
      const options = {}
      for (const key of ['sheet', 'format', 'delimiter', 'header', 'skip', 'dedupe', 'dropConstant', 'maxRows', 'maxFileBytes']) {
        if (args[key] !== undefined) options[key] = args[key]
      }
      const data = await runR('ingest', run.dir, payloadFor(run.dir, run.runId, {
        title: args.title ?? run.title,
        paths,
        options,
      }), exec, timeouts.ingest)
      const lines = [
        `采集完成 · run=${run.runId}`,
        `文件 ${asArray(data.files).length} 个 · 原始 ${data.rowsIn} 行 → 清洗后 ${data.rowsOut} 行 × ${data.colsOut} 列`,
        `编码：${asArray(data.encoding).filter(Boolean).join(', ') || '—'}`,
        `目录：${data.runDir}`,
      ]
      const columns = asArray(data.columns).slice(0, 15)
      if (columns.length > 0) {
        lines.push('列：' + columns.map(c => `${c.name}(${c.type}${c.isId ? ',编号' : ''}${c.pctMissing > 0 ? `,缺失${Math.round(c.pctMissing * 100)}%` : ''})`).join(' · '))
      }
      const ingestWarnings = asArray(data.warnings)
      if (ingestWarnings.length) lines.push('警告：' + ingestWarnings.slice(0, 5).join('；'))
      lines.push(`下一步：r_assess { run: "${run.runId}" }`)
      return { summary: lines.join('\n'), runId: run.runId, runDir: data.runDir, rowsOut: data.rowsOut, colsOut: data.colsOut }
    },
    presentCall: args => ({ card: 'generic', title: `Ingest ${Array.isArray(args.paths) ? args.paths.length : 0} file(s)`, kind: 'read', rawInput: args.paths }),
  })))

  ctx.effect(() => ctx.tools.register(defineTool({
    name: 'r_assess',
    description: '对已清洗数据做价值评估闸门：样本量、完整度、信息变异、独立性与冗余、可分析性、解析可信度六个维度，输出 0-100 分与 PROCEED/CONDITIONAL/STOP。STOP 表示没有满足最小样本量的分析方案，后续 r_analyze/r_report 会拒绝执行；不要绕过闸门。',
    parameters: {
      run: { type: 'string', description: '运行标识（runId）或运行目录；默认最近一次运行。' },
    },
    timeoutMs: timeouts.assess,
    async execute(args, exec) {
      const run = await resolveRun(exec, args.run)
      const data = await runR('assess', run.dir, payloadFor(run.dir, run.meta.runId ?? path.basename(run.dir)), exec, timeouts.assess)
      const dims = asArray(data.dimensions).map(d => `${d.label} ${Math.round(d.score * 100)}`).join(' · ')
      const plans = asArray(data.plans).slice(0, 6).map(p => `- ${p.id}（n=${p.n}，可行性 ${Number(p.score).toFixed(2)}）`)
      const lines = [
        `价值评估：${data.decision} · ${Number(data.score).toFixed(1)}/100`,
        `数据：${data.rows} 行 × ${data.cols} 列 · 完整度 ${(data.completeness * 100).toFixed(1)}% · 重复率 ${(data.duplicateRatio * 100).toFixed(1)}%`,
        `维度：${dims}`,
      ]
      const hardStops = asArray(data.hardStops)
      if (hardStops.length) lines.push(`硬性终止：${hardStops.join('；')}`)
      if (plans.length > 0) {
        lines.push('可用方案：')
        lines.push(...plans)
      } else {
        lines.push('可用方案：无')
      }
      const remediation = asArray(data.remediation)
      if (remediation.length) lines.push('修复建议：' + remediation.slice(0, 3).join('；'))
      lines.push(`评估报告：${path.join(run.dir, 'outputs', 'assessment.html')}`)
      if (data.decision === 'STOP') {
        lines.push('结论：停止。低价值数据不生成分析报告；请先补充样本或修复数据后重新采集。')
      } else {
        lines.push(`下一步：r_analyze { run: "${data.runId}" }`)
      }
      return { summary: lines.join('\n'), decision: data.decision, score: data.score, runId: data.runId }
    },
    presentCall: args => ({ card: 'generic', title: 'Assess data value', kind: 'read', rawInput: args.run }),
  })))

  ctx.effect(() => ctx.tools.register(defineTool({
    name: 'r_analyze',
    description: '在价值闸门允许后执行统计分析：组间对比（t/Welch/ANOVA/Kruskal-Wallis）、相关（Pearson/Spearman）、分类关联（卡方/Cramér V）、月度趋势。自动选择参数或非参数检验、报告效应量与 95% CI、BH 多重校正、假设检查，并生成表格与图表。STOP 闸门会拒绝；CONDITIONAL 只做探索性分析。',
    parameters: {
      run: { type: 'string', description: '运行标识或运行目录；默认最近一次运行。' },
      maxPlans: { type: 'number', description: '最多执行的分析方案数，1-10，默认 6。' },
      alpha: { type: 'number', description: '显著性水平，默认 0.05。' },
      planIds: { type: 'array', items: { type: 'string' }, description: '只执行指定的方案 id（来自 r_assess 的 plans）。' },
    },
    timeoutMs: timeouts.analyze,
    async execute(args, exec) {
      const run = await resolveRun(exec, args.run)
      const options = {}
      if (args.maxPlans !== undefined) options.maxPlans = Math.max(1, Math.min(10, Math.trunc(args.maxPlans)))
      if (args.alpha !== undefined) options.alpha = Math.max(0.001, Math.min(0.2, args.alpha))
      if (args.planIds !== undefined) options.planIds = args.planIds
      const data = await runR('analyze', run.dir, payloadFor(run.dir, run.meta.runId ?? path.basename(run.dir), { options }), exec, timeouts.analyze)
      const analyses = asArray(data.analyses)
      const failed = analyses.filter(a => a.error)
      const lines = [
        `分析完成 · run=${data.runId} · 闸门 ${data.assessmentDecision} · α=${data.alpha} · BH 校正`,
        `共 ${analyses.length} 项，失败 ${failed.length} 项`,
      ]
      for (const [index, analysis] of analyses.entries()) {
        lines.push(analysisLine(analysis, index + 1))
      }
      const keyFindings = asArray(data.keyFindings)
      if (keyFindings.length) {
        lines.push('关键发现：')
        for (const finding of keyFindings.slice(0, 3)) lines.push(`- ${finding}`)
      }
      lines.push(`详细结果：${path.join(run.dir, 'outputs', 'analysis.md')}`)
      if (failed.length > 0) lines.push('警告：部分方案失败，报告中会标注。')
      lines.push(`下一步：r_report { run: "${data.runId}" }`)
      return { summary: lines.join('\n'), runId: data.runId, analyses: analyses.length, failed: failed.length }
    },
    presentCall: args => ({ card: 'generic', title: 'Run R statistical analysis', kind: 'other', rawInput: args.run }),
  })))

  ctx.effect(() => ctx.tools.register(defineTool({
    name: 'r_report',
    description: '生成最终报告：自包含 HTML（图片内嵌）、Markdown、Quarto 源文件，并在 pandoc 可用时生成 DOCX。前提是价值闸门不是 STOP 且已完成 r_analyze。报告包含数据来源、清洗日志、价值评估、方法、结果、图表、局限性与可复现性信息。',
    parameters: {
      run: { type: 'string', description: '运行标识或运行目录；默认最近一次运行。' },
      title: { type: 'string', description: '报告标题；默认使用运行标题。' },
    },
    timeoutMs: timeouts.report,
    async execute(args, exec) {
      const run = await resolveRun(exec, args.run)
      const data = await runR('report', run.dir, payloadFor(run.dir, run.meta.runId ?? path.basename(run.dir), {
        title: args.title,
      }), exec, timeouts.report)
      const lines = [
        `报告完成 · run=${data.runId} · 闸门 ${data.decision} · ${Number(data.score).toFixed(1)}/100`,
        `HTML：${path.join(run.dir, data.html)}`,
        `Markdown：${path.join(run.dir, data.markdown)}`,
        `Quarto：${path.join(run.dir, data.qmd)}`,
      ]
      if (data.docx) lines.push(`DOCX：${path.join(run.dir, data.docx)}`)
      const reportWarnings = asArray(data.warnings)
      if (reportWarnings.length) lines.push('警告：' + reportWarnings.slice(0, 3).join('；'))
      lines.push(`RStudio：打开 ${path.join(run.dir, `${data.runId}.Rproj`)}`)
      return { summary: lines.join('\n'), runId: data.runId, html: path.join(run.dir, data.html) }
    },
    presentCall: args => ({ card: 'generic', title: 'Write R analysis report', kind: 'edit', rawInput: args.run }),
  })))

  ctx.effect(() => ctx.tools.register(defineTool({
    name: 'r_run',
    description: '在某个运行目录中用本地 R 执行一段代码（类似 RStudio 控制台），返回 stdout、消息、警告与错误。代码运行在当前用户权限下，不是沙箱；清洗数据已预加载为 clean，run_dir 指向运行目录。适合做临时探索、画图或核对统计结果。',
    parameters: {
      code: { type: 'string', required: true, description: '要执行的 R 代码。' },
      run: { type: 'string', description: '运行标识或运行目录；默认最近一次运行，没有运行则使用 _scratch 目录。' },
      timeoutSec: { type: 'number', description: '超时秒数，默认 60，最大 300。' },
    },
    timeoutMs: timeouts.run,
    async execute(args, exec) {
      let run
      try {
        run = await resolveRun(exec, args.run)
      } catch (error) {
        if (args.run) throw error
        const cwd = await workspaceOf(exec)
        const scratch = path.join(cwd, runsDirName, '_scratch')
        await fs.mkdir(path.join(scratch, 'logs'), { recursive: true })
        run = { dir: scratch, meta: { runId: '_scratch' } }
      }
      const timeoutSec = Math.max(1, Math.min(300, Math.trunc(args.timeoutSec ?? 60)))
      const data = await runR('run', run.dir, payloadFor(run.dir, run.meta.runId ?? path.basename(run.dir), {
        code: args.code,
      }), exec, timeoutSec * 1000 + 15_000)
      const lines = [
        `R 执行${data.success ? '成功' : '失败'} · ${data.cwd}`,
        data.output ? `输出：\n${tail(data.output, 5000)}` : '输出：（无）',
      ]
      const messages = asArray(data.messages)
      const runWarnings = asArray(data.warnings)
      if (messages.length) lines.push(`消息：${messages.join('；')}`)
      if (runWarnings.length) lines.push(`警告：${runWarnings.join('；')}`)
      if (data.error) lines.push(`错误：${data.error}`)
      lines.push(`代码：${data.snippet}`)
      return { summary: lines.join('\n'), success: data.success, snippet: data.snippet }
    },
    presentCall: args => ({ card: 'generic', title: 'Run R code', kind: 'execute', rawInput: { run: args.run, timeoutSec: args.timeoutSec } }),
  })))
}
