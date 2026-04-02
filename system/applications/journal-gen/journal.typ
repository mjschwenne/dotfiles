// journal.typ — Daily journal template for Supernote Nomad (A5)
//
// Usage:
//   typst compile --input data=/path/to/YYYY-MM-DD.json journal.typ YYYY-MM-DD.pdf

#let data = json(sys.inputs.data)

#set page(paper: "a5", margin: (x: 13mm, top: 12mm, bottom: 12mm))
#set text(size: 10pt)
#set par(leading: 0.65em, spacing: 0.9em)

// ── Helpers ───────────────────────────────────────────────────────────────────

#let badge(label) = box(
  fill: luma(220),
  inset: (x: 4pt, y: 2pt),
  radius: 2pt,
  baseline: 15%,
  text(size: 8pt, weight: "bold", label),
)

#show heading: it => {
  stack(
    spacing: 1mm,
    text(size: 12pt, weight: "bold", upper(it.body)),
    line(length: 100%, stroke: 0.5pt),
  )
  v(2mm)
}

#let task-row(task) = {
  let parts = (badge(task.todo),)
  if task.priority != none { parts.push(strong(task.priority)) }
  if task.deadline != none { parts.push(emph(task.deadline)) }
  parts.push(task.heading)
  block(below: 4pt, parts.join(h(5pt)))
}

// 8mm ruled lines for handwriting
#let ruled-lines(n) = {
  v(3mm)
  for i in range(n) {
    line(length: 100%, stroke: 0.3pt)
    if i < n - 1 { v(7.2mm) }
  }
}

// ── Journal ───────────────────────────────────────────────────────────────────

#text(size: 15pt, weight: "bold", data.date)

= Planning
#for task in data.planned { task-row(task) }
#if data.plan_text != "" {
  v(3mm)
  eval(data.plan_text, mode: "markup")
}

= Review
#if data.completed.len() > 0 {
  for task in data.completed { task-row(task) }
} else [
  #text(fill: luma(150), style: "italic")[No completed tasks.]
]

= Work Reflection
#if data.reflection != "" {
  v(3mm)
  eval(data.reflection, mode: "markup")
} else {
  ruled-lines(6)
}

= Plan for Tomorrow
#if data.tomorrow != "" {
  v(3mm)
  eval(data.tomorrow, mode: "markup")
} else {
  ruled-lines(6)
}

= Personal Reflection

#block(height: 1fr, width: 100%, clip: true, ruled-lines(18))

// ── Final page: blank ruled paper ────────────────────────────────────────────
// The Supernote repeats the last page when writing past the end of a PDF,
// so this page should contain nothing but ruled lines.

#pagebreak()
#ruled-lines(18)

