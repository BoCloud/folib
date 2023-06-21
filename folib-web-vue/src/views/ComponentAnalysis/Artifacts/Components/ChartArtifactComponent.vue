<template>
  <div class="wrapper">
    <h5 id="chart-policy-violation-breakdown" class="card-title mb-0">组件统计</h5>
    <p>组件分类</p>
    <div class="progress-group-inner">
      <div class="progress-group">
        <div class="progress-group-header">
          <a-icon type="hdd" />
          <span class="title ml-5">组件总数</span>
          <span class="ml-auto font-weight-bold"
            >{{ components.length }} <span class="text-muted small"></span></span
          >
        </div>
        <div class="progress-group-bars">
          <a-progress :percent="100" strokeColor="#4dbd74" :show-info="false" />
        </div>
      </div>
      <div class="progress-group">
        <div class="progress-group-header">
          <a-icon type="check-circle" />
          <span class="title ml-5">健康组件</span>
          <span class="ml-auto font-weight-bold"
            >{{ healthComponentCount }} <span class="text-muted small">({{ healthComponentPercent }}%)</span></span
          >
        </div>
        <div class="progress-group-bars">
          <a-progress :percent="healthComponentPercent" strokeColor="#4dbd74" :show-info="false" />
        </div>
      </div>
      <div class="progress-group">
        <div class="progress-group-header">
          <a-icon type="close-circle" />
          <span class="title ml-5">风险组件</span>
          <span class="ml-auto font-weight-bold"
            >{{ vulnerabilityComponentCount }} <span class="text-muted small">({{ vulnerabilityComponentPercent }}%)</span></span
          >
        </div>
        <div class="progress-group-bars">
          <a-progress :percent="vulnerabilityComponentPercent" strokeColor="#f86c6b" :show-info="false" />
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { calcProgressPercent } from "@/utils/util";
export default {
  props: {
    metrics: {
      type: Array,
    },
    artifactData: {
			type: Object,
			default: {},
    },
  },
  data() {
    return {
      components: [],
      healthComponentCount: 0,
      healthComponentPercent: 0,
      vulnerabilityComponentCount: 0,
      vulnerabilityComponentPercent: 0,
    };
  },
  watch: {
    artifactData() {
      this.getData();
    },
    deep: true,
  },
  mounted() {
    this.getData();
  },
  methods: {
    getData() {
      if (this.artifactData.artifact.componentSet) {
        this.components = this.artifactData.artifact.componentSet
        let total = this.components.length
        this.healthComponentCount = this.components.filter(item => item.vulnerabilitiesCount === 0).length
        this.vulnerabilityComponentCount = total - this.healthComponentCount
        this.healthComponentPercent = calcProgressPercent(total, this.healthComponentCount)
        this.vulnerabilityComponentPercent = calcProgressPercent(total, this.vulnerabilityComponentCount)
      }
    },
  },
};
</script>

<style lang="scss" scoped>
.wrapper {
  min-height: 354px;
  p {
    font-size: 14px;
    color: #656464;
  }
}
.card-title {
  color: #17232f;
}
.title {
  flex: 1;
}
.progress-group-header {
  display: flex;
  align-items: center;
}
.progress-group-inner {
  padding-top: 50px;
}
.progress-group {
  margin-bottom: 20px;
}
</style>
