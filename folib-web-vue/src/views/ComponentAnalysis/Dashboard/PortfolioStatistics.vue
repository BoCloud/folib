<template>
  <div>
    <a-card :bordered="false" title="投资组合统计">
      <div class="card-inner">
        <div class="bar-card">
          <div class="callout">
            <div class="text">
              <div class="text-muted">项目</div>
              <strong>{{ totalProjects }}</strong>
            </div>
          </div>
        </div>
        <div class="bar-card">
          <div class="callout">
            <div class="text">
              <div class="text-muted">组件</div>
              <strong>{{ totalComponents }}</strong>
            </div>
          </div>
        </div>
        <div class="bar-card">
          <div class="callout">
            <div class="text">
              <div class="text-muted">投资组合脆弱性</div>
              <strong>{{ vulnerabilities }}</strong>
            </div>
          </div>
        </div>
        <div class="bar-card">
          <div class="callout">
            <div class="text">
              <div class="text-muted">抑制</div>
              <strong>{{ suppressed }}</strong>
            </div>
          </div>
        </div>
      </div>
    </a-card>
  </div>
</template>

<script>
import { valueWithDefault } from "@/utils/util.js";
export default {
  props: {
    dayData: {
      type: Array,
      default: () => [], // 设置默认值为空数组
    },
  },
  watch: {
    dayData() {
      this.getDaysData();
    },
  },
  data() {
    return {
      totalProjects: 0,
      totalComponents: 0,
      vulnerabilities: 0,
      suppressed: 0,
    };
  },

  methods: {
    getDaysData() {
      if (!this.dayData || this.dayData.length === 0) {
        return;
      }
      let metric = this.dayData[this.dayData.length - 1]; //Use the most recent metric
      this.totalProjects = valueWithDefault(metric.projects, "0");
      this.totalComponents = valueWithDefault(metric.components, "0");
      this.vulnerabilities = valueWithDefault(metric.vulnerabilities, "0");
      this.suppressed = valueWithDefault(metric.suppressed, "0");
    },
  },
};
</script>

<style lang="scss" scoped>
.card-inner {
  width: 100%;
  display: flex;
  justify-content: space-evenly;
}
.bar-card {
  width: calc(100% / 4);
  .callout {
    position: relative;
    padding: 0 1rem;
    margin: 1rem 0;
    border-left: 4px solid #0b1015;
    border-radius: 0.25rem;
    border-left-color: #6dd9ff;
  }
  strong {
    font-size: 20px;
  }
}
</style>
