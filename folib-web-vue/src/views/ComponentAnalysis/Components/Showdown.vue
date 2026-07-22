<template>
  <div v-html="outputHTML"></div>
</template>

<script>
import DOMPurify from "dompurify";
import { marked } from "marked";

export default {
  name: "Showdown",
  props: {
    markdown: String,
    allowHtml: {
      type: Boolean,
      default: false,
    },
  },
  computed: {
    outputHTML() {
      const html = marked.parse(this.markdown || "", { gfm: true, breaks: true });
      return DOMPurify.sanitize(html);
    },
  },
};
</script>
