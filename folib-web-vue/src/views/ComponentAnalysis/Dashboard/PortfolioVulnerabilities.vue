<template>
  <div class="wrapper">
    <a-card class="wrapper-inner" :bordered="false">
      <p>投资组合脆弱性</p>
      <p>上次测量：{{ lastMeasurement }}</p>
      <div class="echart">
        <canvas ref="chart" class="chart-line-gradient" :style="{ height: 300 + 'px' }"></canvas>
      </div>
      <!-- 进度条 -->
      <div class="progress-inner">
        <a-card :bordered="false" class="card">
          <h6 class="font-regular text-md mb-0" :class="'mb-10'">{{ "易受攻击的项目" }}</h6>
          <strong>{{ vulnerableProjects }} ({{ vulnerableProjectPercent }}%)</strong>
          <a-progress :percent="vulnerableProjectPercent" strokeColor="#4dbd74" :show-info="false" />
        </a-card>

        <a-card :bordered="false" class="card">
          <h6 class="font-regular text-md mb-0" :class="'mb-10'">{{ "已审核的违规行为" }}</h6>
          <strong>{{ auditedViolations }} ({{ auditedViolationsPercent }}%)</strong>
          <a-progress :percent="auditedViolationsPercent" strokeColor="#f86c6b" :show-info="false" />
        </a-card>

        <a-card :bordered="false" class="card">
          <h6 class="font-regular text-md mb-0" :class="'mb-10'">{{ "易受攻击的组件" }}</h6>
          <strong>{{ vulnerableComponents }} ({{ vulnerableComponentPercent }}%)</strong>
          <a-progress :percent="vulnerableComponentPercent" strokeColor="#ffc107" :show-info="false" />
        </a-card>

        <a-card :bordered="false" class="card">
          <h6 class="font-regular text-md mb-0" :class="'mb-10'">{{ "审计结果" }}</h6>
          <strong>{{ auditedFindings }} ({{ auditedFindingPercent }}%)</strong>
          <a-progress :percent="auditedFindingPercent" strokeColor="#20a8d8" :show-info="false" />
        </a-card>
      </div>
    </a-card>
  </div>
</template>

<script>
import { formatTimestamp, calcProgressPercent, valueWithDefault } from "@/utils/util.js";
import { Chart, registerables } from "chart.js";
Chart.register(...registerables);
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
      this.buildData();
    },
  },
  mounted() {
    this.buildData();
  },

  beforeDestroy: function () {
    this.chart.destroy();
  },
  data() {
    return {
      lastMeasurement: "",
      criticalData: [],
      highData: [],
      mediumData: [],
      lowData: [],
      unassignedData: [],
      labels: [],
      totalProjects: 0,
      vulnerableProjects: 0,
      vulnerableProjectPercent: 0,

      totalComponents: 0,
      vulnerableComponents: 0,
      vulnerableComponentPercent: 0,

      totalFindings: 0,
      auditedFindings: 0,
      auditedFindingPercent: 0,

      totalViolations: 0,
      auditedViolations: 0,
      auditedViolationsPercent: 0,
    };
  },
  methods: {
    getDaysData() {
      if (!this.dayData || this.dayData.length === 0) {
        return;
      }
      let metric = this.dayData[this.dayData.length - 1]; //Use the most recent metric
      this.lastMeasurement = formatTimestamp(metric.lastOccurrence, true);

      this.totalProjects = valueWithDefault(metric.projects, "0");
      this.vulnerableProjects = valueWithDefault(metric.vulnerableProjects, "0");
      this.vulnerableProjectPercent = calcProgressPercent(this.totalProjects, this.vulnerableProjects);

      this.totalComponents = valueWithDefault(metric.components, "0");
      this.vulnerableComponents = valueWithDefault(metric.vulnerableComponents, "0");
      this.vulnerableComponentPercent = calcProgressPercent(this.totalComponents, this.vulnerableComponents);

      this.totalFindings = valueWithDefault(metric.findingsTotal, "0");
      this.auditedFindings = valueWithDefault(metric.findingsAudited, "0");
      this.auditedFindingPercent = calcProgressPercent(this.findingsTotal, this.findingsAudited);

      this.totalViolations = valueWithDefault(metric.policyViolationsTotal, "0");
      this.auditedViolations = valueWithDefault(metric.policyViolationsAudited, "0");
      this.auditedViolationsPercent = calcProgressPercent(this.policyViolationsTotal, this.policyViolationsAudited);

      for (let i = 0; i < this.dayData.length; i++) {
        this.labels.push(formatTimestamp(this.dayData[i].firstOccurrence));
        this.criticalData.push(this.dayData[i].critical);
        this.highData.push(this.dayData[i].high);
        this.mediumData.push(this.dayData[i].medium);
        this.lowData.push(this.dayData[i].low);
        this.unassignedData.push(this.dayData[i].unassigned);

        if (i === this.dayData.length - 1) {
          this.labels.push(formatTimestamp(this.dayData[i].lastOccurrence));
          this.criticalData.push(this.dayData[i].critical);
          this.highData.push(this.dayData[i].high);
          this.mediumData.push(this.dayData[i].medium);
          this.lowData.push(this.dayData[i].low);
          this.unassignedData.push(this.dayData[i].unassigned);
        }
      }
    },

    buildData() {
      let ctx = this.$refs.chart.getContext("2d");
      if (this.chart) {
        this.chart.destroy();
      }
      this.chart = new Chart(ctx, {
        type: "line",
        data: {
          labels: this.labels,
          datasets: [
            {
              label: this.dataOneTag,
              tension: 0.4,
              pointRadius: 0,
              borderColor: "#f86c6b",
              borderWidth: 2,
              backgroundColor: "transparent",
              fill: true,
              data: this.criticalData,
              maxBarThickness: 6,
            },
            {
              label: this.dataTwoTag,
              tension: 0.4,
              pointRadius: 0,
              borderColor: "#fd8c00",
              backgroundColor: "transparent",
              borderWidth: 1,
              fill: true,
              data: this.highData,
              maxBarThickness: 6,
            },
            {
              label: this.dataTwoTag,
              tension: 0.4,
              pointRadius: 0,
              borderColor: "#ffc107",
              backgroundColor: "transparent",
              borderWidth: 1,
              fill: true,
              data: this.mediumData,
              maxBarThickness: 6,
            },
            {
              label: this.dataTwoTag,
              tension: 0.4,
              pointRadius: 0,
              borderColor: "#4dbd74",
              backgroundColor: "transparent",
              borderWidth: 1,
              fill: true,
              data: this.lowData,
              maxBarThickness: 6,
            },
            {
              label: this.dataTwoTag,
              tension: 0.4,
              pointRadius: 0,
              borderColor: "#777777",
              backgroundColor: "transparent",
              borderWidth: 1,
              fill: true,
              data: this.unassignedData,
              maxBarThickness: 6,
            },
          ],
        },
        options: {
          layout: {
            padding: {
              top: 10,
              right: 15,
              left: 10,
              bottom: 15,
            },
          },
          //   responsive: true,
          maintainAspectRatio: false,
          plugins: {
            legend: {
              display: false,
            },
          },
          tooltips: {
            mode: "index",
            enabled: false,
            intersect: true,
            mode: "index",
            position: "nearest",
            callbacks: {
              labelColor: function (tooltipItem, chart) {
                return {
                  backgroundColor: chart.data.datasets[tooltipItem.datasetIndex].borderColor,
                };
              },
            },
          },
          scales: {
            y: {
              grid: {
                drawBorder: false,
                display: true,
                drawOnChartArea: true,
                drawTicks: false,
                borderDash: [5, 5],
                ticks: {
                  display: false,
                  min: 0,
                  max: Math.max.apply(Math, this.dataOne) + 5,
                },
              },
              ticks: {
                display: true,
                padding: 10,
                color: "#b2b9bf",
                font: {
                  size: 11,
                  family: "Open Sans",
                  style: "normal",
                  lineHeight: 2,
                },
              },
            },
            x: {
              grid: {
                drawBorder: false,
                display: true,
                drawOnChartArea: true,
                drawTicks: true,
                borderDash: [5, 5],
              },
              ticks: {
                display: true,
                color: "#b2b9bf",
                padding: 10,
                font: {
                  size: 11,
                  family: "Open Sans",
                  style: "normal",
                  lineHeight: 2,
                },
              },
            },
          },
        },
      });
    },
  },
};
</script>

<style lang="scss" scoped>
.wrapper {
  width: 100%;
  margin-top: 20px;
  margin-bottom: 20px;
}
.wrapper-inner {
  width: 100%;
  & p:nth-of-type(1) {
    color: #17232f;
    font-size: 20px;
    font-weight: 600;
  }
  & p:nth-of-type(2) {
    color: #656464;
    font-size: 16px;
  }
}
.progress-inner {
  width: 100%;
  display: flex;
  justify-content: space-between;
  .card {
    width: calc(100% / 4.2);
  }
}
</style>
