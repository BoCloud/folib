<template>
  <div class="wrapper">
    <!-- 项目漏洞 -->
    <a-card :bordered="false" class="vulnerabilities-card" :bodyStyle="{ padding: 0 }" style="margin-bottom: 20px">
      <p>{{ $t("Projects.ProjectVulnerabilities") }}</p>
      <p>
        {{ $t("Projects.LastBOMImport1") }}
        {{ lastBomImport === "n/a" ? "n/a" : formatTimestamp(lastBomImport, true, lang) }}
      </p>
      <p>
        {{ $t("Projects.LastMeasurement") }}
        {{ lastMeasurement === "n/a" ? "n/a" : formatTimestamp(lastMeasurement, true, lang) }}
      </p>
      <div class="echart">
        <canvas ref="chart" class="chart-line-gradient" :style="{ height: 300 + 'px' }"></canvas>
      </div>
      <div class="bar">
        <div class="card-inner">
          <div class="bar-card">
            <div class="callout b-severity-critical">
              <div class="text">
                <div class="text-muted">{{ $t("Projects.Seriously") }}</div>
                <strong>{{ currentCritical }}</strong>
              </div>
            </div>
          </div>
          <div class="bar-card">
            <div class="callout b-severity-high">
              <div class="text">
                <div class="text-muted">{{ $t("Projects.HighRisk") }}</div>
                <strong>{{ currentHigh }}</strong>
              </div>
            </div>
          </div>
          <div class="bar-card">
            <div class="callout b-severity-medium">
              <div class="text">
                <div class="text-muted">{{ $t("Projects.MediumRisk") }}</div>
                <strong>{{ currentMedium }}</strong>
              </div>
            </div>
          </div>
          <div class="bar-card">
            <div class="callout b-severity-low">
              <div class="text">
                <div class="text-muted">{{ $t("Projects.LowRisk") }}</div>
                <strong>{{ currentLow }}</strong>
              </div>
            </div>
          </div>
          <div class="bar-card">
            <div class="callout b-severity-unassigned">
              <div class="text">
                <div class="text-muted">{{ $t("Projects.Unassigned") }}</div>
                <strong>{{ currentUnassigned }}</strong>
              </div>
            </div>
          </div>
          <div class="bar-card">
            <div class="callout b-severity-info">
              <div class="text">
                <div class="text-muted">{{ $t("Projects.RiskScore") }}</div>
                <strong>{{ currentRiskScore }}</strong>
              </div>
            </div>
          </div>
        </div>
      </div>
    </a-card>
    <!-- 违反政策 -->
    <div class="wrapper-com">
      <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 10 }">
        <ChartPolicyViolations :metrics="vulnerabilitiesData"></ChartPolicyViolations>
      </a-card>
      <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 10 }">
        <ChartPolicyViolationBreakdown :metrics="vulnerabilitiesData"></ChartPolicyViolationBreakdown>
      </a-card>
      <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 10 }">
        <ChartComponentVulnerabilities :metrics="vulnerabilitiesData"></ChartComponentVulnerabilities>
      </a-card>
      <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 10 }">
        <ChartAuditingProgress :metrics="vulnerabilitiesData"></ChartAuditingProgress>
      </a-card>
    </div>
  </div>
</template>

<script>
import { getProjectsVulnerabilities } from "@/api/projects.js"
import { formatTimestamp, valueWithDefault } from "@/utils/util.js"
import ChartPolicyViolations from "./Components/ChartPolicyViolations.vue"
import ChartAuditingProgress from "./Components/ChartAuditingProgress.vue"
import ChartComponentVulnerabilities from "./Components/ChartComponentVulnerabilities.vue"
import ChartPolicyViolationBreakdown from "./Components/ChartPolicyViolationBreakdown.vue"
import { Chart, registerables } from "chart.js"
Chart.register(...registerables)
export default {
  components: {
    ChartPolicyViolations,
    ChartComponentVulnerabilities,
    ChartAuditingProgress,
    ChartPolicyViolationBreakdown,
  },
  computed: {
    lang() {
      return this.$store.state.language.lang
    },
  },

  data() {
    return {
      vulnerabilitiesData: [],
      labels: [],
      criticalData: [],
      highData: [],
      mediumData: [],
      lowData: [],
      unassignedData: [],

      currentCritical: 0,
      currentHigh: 0,
      currentMedium: 0,
      currentLow: 0,
      currentUnassigned: 0,
      currentRiskScore: 0,

      lastMeasurement: "n/a",
      lastBomImport: "n/a",
    }
  },
  watch: {
    vulnerabilitiesData: {
      handler(newVal, oldVal) {
        this.buildData()
      },
      deep: true, // 开启深度监听
    },
  },
  mounted() {
    this.buildData()
    this.getProjectsVulnerabilities()
  },

  beforeDestroy: function () {
    this.chart.destroy()
  },

  methods: {
    formatTimestamp,
    getProjectsVulnerabilities() {
      const uuid = this.$route.params.id
      getProjectsVulnerabilities(uuid).then((res) => {
        this.vulnerabilitiesData = res.data

        const metrics = res.data

        if (metrics && metrics.lastBomImport) {
          this.lastBomImport = metrics.lastBomImport
        } else {
          this.lastBomImport = "n/a"
        }
        let metric = metrics[metrics.length - 1] //Use the most recent metric
        this.currentCritical = valueWithDefault(metric.critical, 0)
        this.currentHigh = valueWithDefault(metric.high, 0)
        this.currentMedium = valueWithDefault(metric.medium, 0)
        this.currentLow = valueWithDefault(metric.low, 0)
        this.currentUnassigned = valueWithDefault(metric.unassigned, 0)
        this.currentRiskScore = valueWithDefault(metric.inheritedRiskScore, 0)
        this.lastMeasurement = metric.lastOccurrence

        for (let i = 0; i < metrics.length; i++) {
          this.labels.push(formatTimestamp(metrics[i].firstOccurrence))
          this.criticalData.push(metrics[i].critical)
          this.highData.push(metrics[i].high)
          this.mediumData.push(metrics[i].medium)
          this.lowData.push(metrics[i].low)
          this.unassignedData.push(metrics[i].unassigned)

          if (i === metrics.length - 1) {
            this.labels.push(formatTimestamp(metrics[i].lastOccurrence))
            this.criticalData.push(metrics[i].critical)
            this.highData.push(metrics[i].high)
            this.mediumData.push(metrics[i].medium)
            this.lowData.push(metrics[i].low)
            this.unassignedData.push(metrics[i].unassigned)
          }
        }
      })
    },
    buildData() {
      let ctx = this.$refs.chart.getContext("2d")
      if (this.chart) {
        this.chart.destroy()
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
            position: "nearest",
            callbacks: {
              labelColor: function (tooltipItem, chart) {
                return {
                  backgroundColor: chart.data.datasets[tooltipItem.datasetIndex].borderColor,
                }
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
      })
    },
  },
}
</script>

<style lang="scss" scoped>
::v-deep .part-title {
    font-size: 21px;
    font-weight: 600;
}
::v-deep .part-sub-title {
    font-size: 11px;
    color: rgba(115, 129, 143, 0.7);
}
.callout {
  height: 50px;
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
.bar {
  width: 100%;
  .card-inner {
    width: 100%;
    display: flex;
    justify-content: space-evenly;
  }
}
.bar-card {
  height: 100px;
}
.wrapper-com {
  width: 100%;
  display: flex;
  justify-content: space-between;
  flex-wrap: wrap;
  .header-solid {
    width: calc(100% / 2.03);
    margin-bottom: 20px;
  }
}
.vulnerabilities-card {
  padding: 24px;
  & p:nth-of-type(1) {
    padding: 0 10px;
    font-size: 20px;
    font-weight: 600;
    color: #17232f;
  }
  & p:nth-of-type(2) {
    padding: 0 10px;
    font-size: 14px;
    color: #656464;
    line-height: 26px;
  }
  & p:nth-of-type(3) {
    padding: 0 10px;
    font-size: 14px;
    color: #656464;
    line-height: 26px;
  }
}
</style>
