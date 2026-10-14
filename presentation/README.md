# UF Sports Analytics Lab Quarto Reveal.js theme

<img width="1710" height="985" alt="template_example" src="https://github.com/user-attachments/assets/8b107f94-dc9e-4b0e-8239-2bf315f56ac9" />

A Quarto reveal.js theme in the UF Sports Analytics Lab style. The Quarto Reveal.js backend will render HTML slides. This is great for interactive content and dynamic responsiveness (e.g., automatically fitting whatever display device is being used).

## Installing

Clone the repo, drop the git history, and start editing `template.qmd` with the codeblock below. Don't forget to change my-talk to whatever you actually want your presentation/talk to be called:

```bash
git clone https://github.com/UFSAL/presentation-template.git my-talk
cd my-talk
rm -rf .git
quarto render template.qmd
```

## Using

Set the format in your document's YAML and give it a footer (this is the kicker
text at the top of every slide):

```yaml
---
format:
  ufsal-revealjs:
    footer: "Presentation Title"
---
```

`UF_logo.png`, `SAL_logo.png` and `ubiguity_gus.png` come with the repo and sit next
to your `.qmd`, so the layouts below work without adding any images of your own.

### Standard slide

```markdown
## Whatever you want to put here

- Bullet point 1
- Bullet point 2
```

### Title slide

`{.uf-title}` with a background photo. The orange rule is drawn by the theme; the
two logos are ordinary images tagged `.uf-mark` and `.sal-mark`.

```markdown
## {.uf-title background-image="ubiguity_gus.png" background-size="cover"}

![](UF_logo.png){.uf-mark}

![](SAL_logo.png){.sal-mark}

::: {.title-block}
Title Block
:::

::: {.subtitle-block}
Presentation Title, Date
:::

::: {.presenter-block}
Your Name Here

Job

Affiliation
:::
```

### Blue panel slide

`{.blue-panel}` puts a full-height blue panel down the left. Text in the panel goes
in a `.panel-text` div; everything else sits in the white area to the right.

```markdown
## There is a picture on this slide {.blue-panel}

::: {.panel-text}
It's the ubiguity statue but you can change it if you want
:::

![](_extensions/ufsal/ubiguity_gus.png){.nostretch width="60%"}
```

Use `.nostretch` on figures here, otherwise reveal stretches a lone image to fill
the slide and it runs over the panel and the bottom bar.

### Thank-you slide

`{.thank-you}` draws the vertical blue gradient (core blue into dark blue, at 90
degrees per the UF brand guide) and drops the bar, logo, kicker and slide number.

```markdown
## Thank you! {.thank-you}

:::: {.columns .contact}

::: {.column width="50%"}
[linkedin_username](https://linkedin.com/in/linkedin_username)
:::

::: {.column width="50%"}
Your Name Goes Here
:::

::::
```
