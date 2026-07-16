<template>
  <div>
    <!-- Bubble chart -->
    <canvas ref="chart" class="chart-bubble" :style="{ height: height + 'px' }"></canvas>
    <!-- / Bubble chart -->
  </div>
</template>

<script>
import { Chart, registerables } from "chart.js";
Chart.register(...registerables);
import { concatenateComponentName } from "@/utils/util";
export default {
  props: {
    chartData: {
      type: Array,
      default: () => [], // 设置默认值为空数组
    },
  },
  watch: {
    chartData() {
      this.getData();
      this.initChart();
    },
  },

  data() {
    return {
      height: 300,
      labels: [],
      cveData: [],
    };
  },
  mounted() {
    this.initChart();
  },
  beforeDestroy: function () {
    this.chart.destroy();
  },
  methods: {
    getData() {
      const findings = this.chartData;
      if (findings && findings.length > 0) {
        for (let i = 0; i < findings.length; i++) {
          let finding = findings[i];
          let componentLabel = concatenateComponentName(null, finding.component.name, finding.component.version);
          this.labels.push({ vulnId: finding.vulnerability.vulnId, componentLabel: componentLabel });
          let cvssScore = finding.vulnerability.cvssV3BaseScore ? finding.vulnerability.cvssV3BaseScore : finding.vulnerability.cvssV2BaseScore;
          this.cveData.push({ x: cvssScore, y: finding.vulnerability.epssScore });
        }
      }
    },
    initChart() {
      let ctx = this.$refs.chart.getContext("2d");
      if (this.chart) {
        this.chart.destroy();
      }
      this.chart = new Chart(ctx, {
        type: "bubble",
        data: {
          labels: this.labels,
          datasets: [
            {
              label: "",
              data: this.cveData,
              backgroundColor: "#A66BF8",
            },
          ],
        },
        options: {
          layout: {
            padding: {
              top: 30,
              right: 15,
              left: 10,
              bottom: 5,
            },
          },
          responsive: true,
          maintainAspectRatio: false,
          plugins: {
            legend: {
              display: false,
            },
          },
          tooltips: {
            enabled: true,
            mode: "index",
            intersect: false,
          },
          scales: {
            y: {
              grid: {
                drawBorder: false,
                display: true,
                drawOnChartArea: true,
                drawTicks: false,
                borderDash: [5, 5],
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

<style lang="scss" scoped></style>
