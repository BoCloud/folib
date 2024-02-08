<template>
  <div v-if="text && text.length > max">
        <span
            v-if="notool"
            class="text_over"
        >
            {{ text.substr(0,max) }}...
        </span>
    <a-tooltip
        v-else
        :title="text"
        placement="topLeft"
    >
            <span class="text_over">
                {{ text.substr(0,max) }}...
            </span>
    </a-tooltip>
  </div>

  <div v-else>
    <span>{{ text }}</span>
  </div>
</template>
<script>
export default {
  props: {
    text: {
      default: '',
    },
    notool: {
      type: Boolean,
      default: false,
    },
    max: {
      default: 6,
    },
  },
  data() {
    return {
      showLimit: 10,
    }
  },
  watch: {
    max: {
      handler(val) {
        const str = this.text.substr(0, val)
        if (/[a-z]/i.test(str)) {
          const L = str.match(/[a-z]/gi).length
          this.showLimit = val + L
          const strNew = this.text.substr(val, this.showLimit)
          if (/[a-z]/i.test(strNew)) {
            const l = strNew.match(/[a-z]/gi).length
            this.showLimit -= L - l
          }
        } else {
          this.showLimit = val
        }
      },
    },
  },
}
</script>
<style lang="scss" scoped>

</style>
