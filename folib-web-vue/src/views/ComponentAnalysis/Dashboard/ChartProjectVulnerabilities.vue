<template>
  <div>
    <a-card :bordered="false">
      <h6 class="font-regular text-md mb-0" :class="'mb-10'">{{ "项目" }}</h6>
      <div class="echart">
        <canvas ref="chart" class="chart-line-gradient" :style="{ height: 200 + 'px' }"></canvas>
      </div>
    </a-card>
  </div>
</template>

<script>
import { formatTimestamp } from "@/utils/util.js";
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
      labels: [],
      totalData: [],
      affectedData: [],
    };
  },

  methods: {
    getDaysData() {
      if (!this.dayData || this.dayData.length === 0) {
        return;
      }
      for (let i = 0; i < this.dayData.length; i++) {
        this.labels.push(formatTimestamp(this.dayData[i].firstOccurrence));
        this.totalData.push(this.dayData[i].projects);
        this.affectedData.push(this.dayData[i].vulnerableProjects);

        if (i === this.dayData.length - 1) {
          this.labels.push(formatTimestamp(this.dayData[i].lastOccurrence));
          this.totalData.push(this.dayData[i].projects);
          this.affectedData.push(this.dayData[i].vulnerableProjects);
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
              label: "条数",
              tension: 0.4,
              pointRadius: 0,
              borderColor: "#f86c6b",
              borderWidth: 2,
              backgroundColor: "transparent",
              fill: true,
              data: this.totalData,
              maxBarThickness: 6,
            },
            {
              label: "易受攻击",
              tension: 0.4,
              pointRadius: 0,
              borderColor: "#20a8d8",
              backgroundColor: "transparent",
              borderWidth: 1,
              fill: true,
              data: this.affectedData,
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
