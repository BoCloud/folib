<template>
  <div>
    <a-card :bordered="false" style="margin-bottom: 20px">
      <a-row style="display: flex; justify-content: space-between">
        <a-col style="flex: 1; display: flex">
          <a-icon
            style="
              width: 56px;
              height: 56px;
              font-size: 30px;
              background-color: #20a8d8;
              color: #fff;
              line-height: 60px;
              float: left;
              margin-right: 10px;
            "
            type="apartment"
          />
          <div class="title">
            <span class="h5">
              {{ vulnerability.cve }}
            </span>
            <span style="display: block">
              {{ 'NVD' }}
            </span>
          </div>
        </a-col>
        <a-col style="display: flex; align-items: center">
          <a-avatar :size="24" :src="'images/folib/' + severityColor.toLowerCase() + '.svg'" />
          <div class="text-center pull-left severity-bug-label">
            <div style="font-size: 20px; padding: 6px">
              {{ severityLabel }}
            </div>
          </div>
        </a-col>
      </a-row>
    </a-card>

    <a-tabs class="tabs-sliding" default-active-key="1" @change="handleChangeTabs">
      <a-tab-pane key="1" :tab="$t('Vulnerabilities.GeneralView')">
        <VulnerabilitiesOverView v-if="tabActive == 1" :vulnerability="vulnerability"></VulnerabilitiesOverView>
      </a-tab-pane>
      <a-tab-pane key="2" :tab="$t('Vulnerabilities.AffectedProducts')">
        <AffectedArtifacts v-if="tabActive == 2"></AffectedArtifacts>
      </a-tab-pane>
    </a-tabs>
  </div>
</template>

<script>
import { getVulnerabilityDetail } from "@/api/vulnerabilities.js";
import { valueWithDefault, formatTimestamp } from "@/utils/util";
import VulnerabilitiesOverView from "./VulnerabilitiesOverView.vue";
import AffectedArtifacts from "./AffectedArtifacts.vue";
export default {
  components: { VulnerabilitiesOverView, AffectedArtifacts },

  data() {
    return {
      tabActive: 1,
      vulnerability: {},
      source: null,
      vulnId: null,
    };
  },
  created() {
    this.initialize();
  },

  computed: {
    severityLabel() {
      switch (this.vulnerability.severity) {
        case "CRITICAL":
          return this.$t('Vulnerabilities.Seriously');
        case "HIGH":
          return this.$t('Vulnerabilities.HighRisk');
        case "MEDIUM":
          return this.$t('Vulnerabilities.MediumRisk');
        case "LOW":
          return this.$t('Vulnerabilities.LowRisk');
        case "INFO":
          return this.$t('Vulnerabilities.Information');
        default:
          return this.$t('Vulnerabilities.Unassigned');
      }
    },
    severityColor() {
      switch (this.vulnerability.severity) {
        case "CRITICAL":
          return "CRITICAL";
        case "HIGH":
          return "HIGH";
        case "MEDIUM":
          return "MEDIUM";
        case "LOW":
          return "LOW";
        case "INFO":
          return "INFO";
        default:
          return "unassigned";
      }
    },
  },
  methods: {
    initialize() {
      const id = this.$route.params.id;
      getVulnerabilityDetail(id).then((res) => {
        this.vulnerability = {}
        if (res) {
          this.vulnerability = res
        }
      });
    },
    handleChangeTabs(val) {
      this.tabActive = val;
    },
  },
};
</script>

<style lang="scss" scoped>
.title {
  margin-top: 2px;
}
.dropdown {
  margin-left: 10px;
}

// ::v-deep .vue-easy-pie-chart .inner-text {
//   position: absolute;
//   top: 0;
//   left: 0;
//   right: 0;
//   bottom: 0;
//   text-align: center;
//   display: block;
// }

::v-deep .ant-card-head {
  padding: 0 !important;
}
::v-deep .ant-tabs-nav-wrap {
  padding: 0 24px;
}
.text-muted {
  display: block;
}
</style>
