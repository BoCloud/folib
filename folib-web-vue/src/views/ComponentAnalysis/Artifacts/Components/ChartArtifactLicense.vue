<template>
  <div class="wapper">
    <p>License</p>
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
    artifactData: {
      type: Object,
      default: {},
    },
  },
  watch: {
    artifactData() {
      this.getData();
    },
    deep: true,
  },
  mounted() {
    this.getData()
  },
  beforeDestroy: function () {
    // this.chart.destroy();
  },
  data() {
    return {
      licenseData: [],
      licenseXData: [],
      licenseYData: [],
    };
  },
  methods: {
    getData() {
      if (!this.artifactData.artifact.componentSet) {
        return
      }
      this.artifactData.artifact.componentSet.forEach(item => {
        if (item.license && item.license.length > 0) {
          item.license.forEach(l => {
            if (this.licenseData) {
              let existsLicense = this.licenseData.filter(license => license.licenseId === l)
              if (existsLicense && existsLicense.length > 0) {
                existsLicense[0].count = existsLicense[0].count + 1
              } else {
                this.licenseData.push({ licenseId: l, count: 1 })
              }
            }
          })
        } else {
          let licenseId = '无License'
          let existsLicense = this.licenseData.filter(license => license.licenseId === licenseId)
          if (existsLicense && existsLicense.length > 0) {
            existsLicense[0].count = existsLicense[0].count + 1
          } else {
            this.licenseData.push({ licenseId: licenseId, count: 1 })
          }
        }
      });
      this.licenseData.forEach(item => {
        this.licenseXData.push(item.licenseId)
        this.licenseYData.push(item.count)
      })
      this.buildData()
    },
    buildData() {
      let ctx = this.$refs.chart.getContext("2d");
      if (this.chart) {
        this.chart.destroy();
      }

      this.chart = new Chart(ctx, {
        type: "bar",
        data: {
          labels: this.licenseXData,
          datasets: [{
            label: "数量",
            weight: 5,
            borderWidth: 0,
            borderRadius: 4,
            backgroundColor: '#4dbd74',
            data: this.licenseYData,
            fill: false,
            maxBarThickness: 35
          }],
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
                borderDash: [5, 5]
              },
              ticks: {
                display: true,
                padding: 10,
                color: '#b2b9bf',
                font: {
                  size: 11,
                  family: "Open Sans",
                  style: 'normal',
                  lineHeight: 2
                },
              },
            },
            x: {
              grid: {
                drawBorder: false,
                display: true,
                drawOnChartArea: true,
                drawTicks: true,
                borderDash: [5, 5]
              },
              ticks: {
                display: true,
                color: '#b2b9bf',
                padding: 10,
                font: {
                  size: 11,
                  family: "Open Sans",
                  style: 'normal',
                  lineHeight: 2
                },
              },
            },
          },
        }
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
    font-size: 16px;
    color: #17232f;
  }
}
</style>
