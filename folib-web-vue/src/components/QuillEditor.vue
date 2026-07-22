<template>
  <div ref="editorContainer"></div>
</template>

<script>
import Quill from "quill";
import "quill/dist/quill.snow.css";

export default {
  name: "QuillEditor",
  props: {
    value: { type: String, default: "" },
    options: { type: Object, default: () => ({}) },
    disabled: { type: Boolean, default: false },
  },
  data() {
    return { quill: null };
  },
  mounted() {
    this.quill = new Quill(this.$refs.editorContainer, {
      theme: "snow",
      readOnly: this.disabled,
      ...this.options,
    });
    if (this.value) {
      this.quill.root.innerHTML = this.value;
    }
    this.quill.on("text-change", () => {
      this.$emit("input", this.quill.root.innerHTML);
    });
  },
  watch: {
    value(newVal) {
      if (this.quill && newVal !== this.quill.root.innerHTML) {
        this.quill.root.innerHTML = newVal || "";
      }
    },
    disabled(newVal) {
      if (this.quill) {
        this.quill.enable(!newVal);
      }
    },
  },
  beforeDestroy() {
    this.quill = null;
  },
};
</script>
