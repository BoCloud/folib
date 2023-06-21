<template>
  <div class="wrapper">
    <div class="bar">
      <a-card :bordered="false" class="bar-card">
        <div class="count">
          {{ portfolioVulnerabilities }}
        </div>
        <p>
          {{ "投资组合漏洞" }}
        </p>
        <ChartLineGradient
          :dataOne="portfolioVulnerabilitiesData"
          :labels="labels"
          dataOneTag="Portfolio"
          :height="100"
          :gradientColor="gradientColor1"
          :borderColor="borderColor1"
          :isDisplay="false"
        ></ChartLineGradient>
      </a-card>
      <a-card :bordered="false" class="bar-card">
        <div class="count">
          {{ vulnerableProjects }}
        </div>
        <p>
          {{ "风险项目" }}
        </p>
        <ChartLineGradient
          :dataOne="vulnerableProjectsData"
          :labels="labels"
          dataOneTag="Projects"
          :height="100"
          :gradientColor="gradientColor2"
          :borderColor="borderColor2"
          :isDisplay="false"
        ></ChartLineGradient>
      </a-card>
      <a-card :bordered="false" class="bar-card">
        <div class="count">
          {{ vulnerableComponents }}
        </div>
        <p>
          {{ "漏洞组件" }}
        </p>
        <ChartLineGradient
          :dataOne="vulnerableComponentsData"
          :labels="labels"
          dataOneTag="Vulnerable"
          :height="100"
          :gradientColor="gradientColor3"
          :borderColor="borderColor3"
          :isDisplay="false"
        ></ChartLineGradient>
      </a-card>
      <a-card :bordered="false" class="bar-card">
        <div class="count">
          {{ inheritedRiskScore }}
        </div>
        <p>
          {{ "继承风险评分" }}
        </p>
        <ChartLineGradient
          :dataOne="inheritedRiskScoreData"
          :labels="labels"
          dataOneTag="Inherited"
          :height="100"
          :gradientColor="gradientColor4"
          :borderColor="borderColor4"
          :isDisplay="false"
        ></ChartLineGradient>
      </a-card>
    </div>
  </div>
</template>

<script>
import ChartLineGradient from "@/components/Charts/ChartLineGradient";
import { getDaysData } from "@/api/dashboard.js";

export default {
  components: { ChartLineGradient },
  watch: {},
  created() {
    this.getDaysData();
  },
  data() {
    return {
      portfolioVulnerabilities: 0,
      vulnerableProjects: 0,
      vulnerableComponents: 0,
      inheritedRiskScore: 0,
      portfolioVulnerabilitiesData: [],
      vulnerableProjectsData: [],
      vulnerableComponentsData: [],
      inheritedRiskScoreData: [],
      labels: [0, 1, 2],
      // 渐变颜色
      gradientColor1: ["rgba(109,217,255,.3)", "rgba(109,217,255,0)", "rgba(109,217,255,0)"],
      gradientColor2: ["rgba(111,66,193,.3)", "rgba(111,66,193,0)", "rgba(111,66,193,0)"],
      gradientColor3: ["rgba(123,104,238,.3)", "rgba(123,104,238,0)", "rgba(123,104,238,0)"],
      gradientColor4: ["rgba(248,108,107,.3)", "rgba(248,108,107,0)", "rgba(248,108,107,0)"],
      // 线条颜色
      borderColor1: "#20a8d8",
      borderColor2: "#9932cc",
      borderColor3: "#7b68ee",
      borderColor4: "#f86c6b",
    };
  },

  methods: {
    getDaysData() {
      getDaysData().then((res) => {
        this.dayData = res.data;
        this.portfolioVulnerabilities = this.dayData[this.dayData.length - 1].vulnerabilities;
        this.vulnerableProjects = this.dayData[this.dayData.length - 1].vulnerableProjects;
        this.vulnerableComponents = this.dayData[this.dayData.length - 1].vulnerableComponents;
        this.inheritedRiskScore = this.dayData[this.dayData.length - 1].inheritedRiskScore;
        for (let i = 0; i < this.dayData.length; i++) {
          this.portfolioVulnerabilitiesData.push(this.dayData[i].vulnerabilities);
          this.vulnerableProjectsData.push(this.dayData[i].vulnerableProjects);
          this.vulnerableComponentsData.push(this.dayData[i].vulnerableComponents);
          this.inheritedRiskScoreData.push(this.dayData[i].inheritedRiskScore);
        }
      });
    },
  },
};
</script>

<style lang="scss" scoped>
.wrapper {
  width: 100%;
}
.bar {
  width: 100%;
  display: flex;
  justify-content: space-between;
  flex-wrap: wrap;
}
.bar-card {
  width: calc(100% / 4.2);
  height: 180px;
  background-color: #fff;
  .count {
    color: #17232f;
    font-size: 20px;
    font-weight: 600;
  }
  p {
    color: #17232f;
    font-size: 16px;
  }
}
</style>
