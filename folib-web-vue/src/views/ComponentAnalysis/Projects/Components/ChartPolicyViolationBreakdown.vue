<template>
  <div class="wrapper">
    <div id="chart-policy-violation-breakdown" class="part-title">{{ $t("Projects.PolicyViolations") }}</div>
    <div class="part-sub-title">{{ $t("Projects.PolicyViolationsByClassification") }}</div>
    <div class="progress-group-inner">
      <div class="progress-group">
        <div class="progress-group-header">
          <a-icon type="alert" />
          <span class="title">{{ $t("Projects.SecurityRisk") }}</span>
          <span class="ml-auto font-weight-bold"
            >{{ securityCount }} <span class="text-muted small">({{ securityPercent }}%)</span></span
          >
        </div>
        <div class="progress-group-bars">
          <a-progress :percent="securityPercent" strokeColor="#4dbd74" :show-info="false" />
        </div>
      </div>
      <div class="progress-group">
        <div class="progress-group-header">
          <a-icon type="trophy" />
          <span class="title">{{ $t("Projects.LicenseRisk") }}</span>
          <span class="ml-auto font-weight-bold"
            >{{ licenseCount }} <span class="text-muted small">({{ licensePercent }}%)</span></span
          >
        </div>
        <div class="progress-group-bars">
          <a-progress :percent="licensePercent" strokeColor="#4dbd74" :show-info="false" />
        </div>
      </div>
      <div class="progress-group">
        <div class="progress-group-header">
          <a-icon type="select" />
          <span class="title">{{ $t("Projects.OperationalRisk") }}</span>
          <span class="ml-auto font-weight-bold"
            >{{ operationalCount }} <span class="text-muted small">({{ operationalPercent }}%)</span></span
          >
        </div>
        <div class="progress-group-bars">
          <a-progress :percent="operationalPercent" strokeColor="#4dbd74" :show-info="false" />
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { calcProgressPercent } from "@/utils/util"
export default {
  props: {
    metrics: {
      type: Array,
    },
  },
  data() {
    return {
      securityCount: 0,
      securityPercent: 0,
      licenseCount: 0,
      licensePercent: 0,
      operationalCount: 0,
      operationalPercent: 0,
    }
  },
  methods: {
    getData() {
      const metrics = this.metrics
      for (let i = 0; i < metrics.length; i++) {
        if (i === metrics.length - 1) {
          const total = metrics[i].policyViolationsTotal
          this.securityCount = metrics[i].policyViolationsSecurityTotal
          this.licenseCount = metrics[i].policyViolationsLicenseTotal
          this.operationalCount = metrics[i].policyViolationsOperationalTotal
          this.securityPercent = calcProgressPercent(total, this.securityCount)
          this.licensePercent = calcProgressPercent(total, this.licenseCount)
          this.operationalPercent = calcProgressPercent(total, this.operationalCount)
        }
      }
    },
  },
}
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
  margin-bottom: 20px;
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
  min-height: 300px;
}
.progress-group {
  margin-bottom: 20px;
}
</style>
