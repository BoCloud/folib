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
          <div class="h5 title">
            {{ componentLabel }}
            <div class="license" v-if="component.license">
              {{ component.license.join(",") }}
            </div>
          </div>
        </a-col>
        <a-col style="display: flex">
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityUnassigned"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'未分配'"
            >{{ component.vulnerabilitiesCount }}</vue-easy-pie-chart
          >
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityCritical"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'危急'"
            >{{ component.criticalVulnerabilitiesCount }}</vue-easy-pie-chart
          >
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityHigh"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'高'"
            >{{ component.highVulnerabilitiesCount }}</vue-easy-pie-chart
          >
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityMedium"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'中危'"
            >{{ component.mediumVulnerabilitiesCount }}</vue-easy-pie-chart
          >
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityLow"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'低'"
            >{{ component.lowVulnerabilitiesCount }}</vue-easy-pie-chart
          >
        </a-col>
      </a-row>
    </a-card>

    <!-- <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 0 }"> -->
    <a-tabs class="tabs-sliding" default-active-key="1" @change="handleChangeTabs">
      <a-tab-pane key="1" tab="总览">
        <ComponentDashboard :component="component" v-if="tabActive == 1"></ComponentDashboard>
      </a-tab-pane>
      <a-tab-pane key="2" tab="漏洞">
        <ComponentVulnerabilities :component="component" v-if="tabActive == 2"></ComponentVulnerabilities>
      </a-tab-pane>
    </a-tabs>
    <!-- </a-card> -->
  </div>
</template>

<script>
import { getComponentsDetail, getComponentsCurrent } from "@/api/module.js";
import VueEasyPieChart from "vue-easy-pie-chart";
import { valueWithDefault } from "@/utils/util";
import "vue-easy-pie-chart/dist/vue-easy-pie-chart.css";
import ComponentVulnerabilities from "./ComponentVulnerabilities.vue";
import ComponentDashboard from "./ComponentDashboard.vue";
export default {
  components: { VueEasyPieChart, ComponentDashboard, ComponentVulnerabilities },

  data() {
    return {
      tabActive: 1,
      component: {},
      severityCritical: "#f86c6b",
      severityHigh: "#fd8c00",
      severityMedium: "#ffc107",
      severityLow: "#4dbd74",
      severityUnassigned: "#777777",
      severityInfo: "#20a8d8",
      trackColor: "#17232f",
    };
  },
  created() {
    this.initialize();
  },

  computed: {
    componentLabel() {
      if (this.component.name && this.component.version) {
        return this.component.name + " ▸ " + this.component.version;
      } else {
        return this.component.name;
      }
    },
  },
  methods: {
    initialize() {
      const uuid = this.$route.params.id;
      getComponentsDetail({uuid:uuid}).then((res) => {
        this.component = res;
      });
    },

    handleClickMenu(p) {
      this.$router.push(`/artifactsDetail/${p.uuid}`);
      this.initialize();
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

.license {
  font-size: 14px;
  font-weight: 100;
  margin-top: 5px;
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
</style>
