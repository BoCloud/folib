<template>
  <div v-html="outputHTML"></div>
</template>

<script>
import DOMPurify from "dompurify";
import showdown from "showdown";
import * as showdownHtmlEscape from "showdown-htmlescape";

export default {
  name: "Showdown",
  props: {
    markdown: String,
    allowHtml: {
      type: Boolean,
      default: false,
    },
    flavor: {
      type: String,
      default: "github",
    },
  },
  computed: {
    converter() {
      const c = new showdown.Converter();
      if (!this.allowHtml) {
        // Escape HTML tags outside of code blocks (Showdown will encode HTML tags within
        // code blocks automatically). This prevents unintended rendering of HTML elements,
        // but is not a security mechanism. Purely visual.
        c.addExtension(showdownHtmlEscape, "html-escape");
      }
      c.setFlavor(this.flavor);
      return c;
    },
    outputHTML() {
      const html = this.converter.makeHtml(this.markdown);
      return DOMPurify.sanitize(html);
    },
  },
};
</script>
