<template>
  <div style="margin-bottom: 20px">
    <HeaderEcharts></HeaderEcharts>
    <PortfolioVulnerabilities :dayData="dayData"></PortfolioVulnerabilities>
    <!-- 四个折线图 -->
    <div class="lineChart-inner">
      <ChartPolicyViolations class="chart-card" :dayData="dayData"></ChartPolicyViolations>
      <ChartAuditingProgress class="chart-card" :dayData="dayData"></ChartAuditingProgress>
      <ChartProjectVulnerabilities class="chart-card" :dayData="dayData"></ChartProjectVulnerabilities>
      <ChartComponentVulnerabilities class="chart-card" :dayData="dayData"></ChartComponentVulnerabilities>
    </div>
    <!-- 投资组合统计 -->
    <PortfolioStatistics :dayData="dayData"></PortfolioStatistics>
  </div>
</template>

<script>
import { getDaysData } from "@/api/dashboard.js";
import HeaderEcharts from "../Components/HeaderEcharts";
import PortfolioVulnerabilities from "./PortfolioVulnerabilities.vue";
import ChartPolicyViolations from "./ChartPolicyViolations.vue";
import ChartArtifactLChartAuditingProgressicense from "./ChartAuditingProgress.vue";
import ChartProjectVulnerabilities from "./ChartProjectVulnerabilities.vue";
import ChartComponentVulnerabilities from "./ChartComponentVulnerabilities.vue";
import PortfolioStatistics from "./PortfolioStatistics.vue";
export default {
  components: {
    HeaderEcharts,
    PortfolioVulnerabilities,
    ChartPolicyViolations,
    ChartAuditingProgress,
    ChartProjectVulnerabilities,
    ChartComponentVulnerabilities,
    PortfolioStatistics,
  },
  created() {
    this.getDaysData();
  },
  data() {
    return {
      dayData: [],
    };
  },

  methods: {
    getDaysData() {
      getDaysData().then((res) => {
        this.dayData = res.data;
      });
    },
  },
};
</script>

<style lang="scss" scoped>
.lineChart-inner {
  width: 100%;
  display: flex;
  justify-content: space-between;
  flex-wrap: wrap;
  .chart-card {
    width: calc(100% / 2.03);
    margin-bottom: 20px;
  }
}
</style>
