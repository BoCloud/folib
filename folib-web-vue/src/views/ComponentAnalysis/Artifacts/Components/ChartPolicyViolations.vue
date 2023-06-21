<template>
  <div class="wapper">
    <p>违反政策</p>
    <p>政策违规情况</p>
    <div class="chart">
      <canvas ref="chart" class="chart-line-gradient" :style="{ height: 300 + 'px' }"></canvas>
    </div>
  </div>
</template>

<script>
import { Chart, registerables } from "chart.js";
Chart.register(...registerables);
import { formatTimestamp } from "@/utils/util";
export default {
  props: {
    metrics: {
      type: Array,
    },
  },
  watch: {
    metrics() {
      this.getData();
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
      labels: [],
      failData: [],
      warnData: [],
      infoData: [],
    };
  },
  methods: {
    getData() {
      const metrics = this.metrics;
      for (let i = 0; i < metrics.length; i++) {
        this.labels.push(formatTimestamp(metrics[i].firstOccurrence));
        this.failData.push(metrics[i].policyViolationsFail);
        this.warnData.push(metrics[i].policyViolationsWarn);
        this.infoData.push(metrics[i].policyViolationsInfo);

        if (i === metrics.length - 1) {
          this.labels.push(formatTimestamp(metrics[i].lastOccurrence));
          this.failData.push(metrics[i].policyViolationsFail);
          this.warnData.push(metrics[i].policyViolationsWarn);
          this.infoData.push(metrics[i].policyViolationsInfo);
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
              data: this.failData,
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
              data: this.warnData,
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
              data: this.infoData,
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
  & p:nth-of-type(1) {
    font-size: 20px;
    font-weight: 600;
    color: #17232f;
  }
  & p:nth-of-type(2) {
    font-size: 14px;
    color: #656464;
  }
}
</style>
