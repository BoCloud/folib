<template>
  <div class="wrapper">
    <div v-if="vulnerability.description" style="margin-bottom: 20px">
      <a-card :title="$t('Vulnerabilities.Overview')">
        <div v-html="vulnerability.description" style="margin-bottom: 20px"></div>
        <div v-if="cvssBaseScore > 0">
          <a-row class="text-center">
            <a-col class="mb-sm-2 mb-0" :xs="{ span: 5, offset: 1 }" :lg="{ span: 6, offset: 1 }">
              <div class="text-muted">{{ $t('Vulnerabilities.basicScore') }}</div>
              <strong>{{ cvssBaseScore }}</strong>
              <a-progress :percent="(cvssBaseScore / 10) * 100" :show-info="false" strokeColor="#fd8c00"></a-progress>
            </a-col>
            <a-col class="mb-sm-2 mb-0 d-sm-down-none" :xs="{ span: 5, offset: 1 }" :lg="{ span: 6, offset: 2 }">
              <div class="text-muted">{{ $t('Vulnerabilities.impactScore') }}</div>
              <strong>{{ cvssImpactScore }}</strong>
              <a-progress :percent="(cvssImpactScore / 10) * 100" :show-info="false" strokeColor="#20a8d8"></a-progress>
            </a-col>
            <a-col class="mb-sm-2 mb-0 d-sm-down-none" :xs="{ span: 5, offset: 1 }" :lg="{ span: 6, offset: 2 }">
              <div class="text-muted">{{ $t('Vulnerabilities.availabilityScore') }}</div>
              <strong>{{ cvssExploitScore }}</strong>
              <a-progress :percent="(cvssExploitScore / 10) * 100" :show-info="false" strokeColor="#20a8d8"></a-progress>
            </a-col>
          </a-row>
        </div>
      </a-card>
    </div>
    <div v-if="vulnerability.cweList" style="margin-bottom: 20px">
      <a-card :title="$t('Vulnerabilities.VulnerabilityType')">
        <div v-for="(item, index) in vulnerability.cweList" :key="index">
          <a :href="cweLink(item)" target="_blank" rel="noopenner noreferrer">{{ item }}</a>
        </div>
      </a-card>
    </div>

    <div v-if="vulnerability.references" style="margin-bottom: 20px">
      <a-card :title="$t('Vulnerabilities.Reference')">
        <div v-for="(item, index) in vulnerability.references" :key="index">
          <a :href="item" target="_blank" rel="noopenner noreferrer" > {{ item }} </a>
        </div>
      </a-card>
    </div>
  </div>
</template>

<script>
import { valueWithDefault, formatTimestamp } from "@/utils/util";
import Showdown from "../Components/Showdown.vue";
export default {
  components: { Showdown },
  props: {
    vulnerability: {
      type: Object,
    },
  },
  computed: {
    cvssBaseScore() {
      return valueWithDefault(this.vulnerability.v3BaseScore ? this.vulnerability.v3BaseScore : this.vulnerability.v2Score, 0);
    },
    cvssImpactScore() {
      return valueWithDefault(
        this.vulnerability.v3ImpactScore ? this.vulnerability.v3ImpactScore : this.vulnerability.v2ImpactScore,
        0
      );
    },
    cvssExploitScore() {
      return valueWithDefault(
        this.vulnerability.v3ExploitabilityScore
          ? this.vulnerability.v3ExploitabilityScore
          : this.vulnerability.v2ExploitabilityScore,
        0
      );
    },
  },
  data() {
    return {};
  },
  methods: {
    cweLink: function (cwe) {
      cwe = cwe.split("-")[1]
      return `https://cwe.mitre.org/data/definitions/${cwe}`;
    },
    cweLabel: function (cwe) {
      return `CWE-${cwe.cweId}: ${cwe.name}`;
    },
  },
};
</script>

<style lang="scss" scoped>
::v-deep .ant-card-head-title {
  padding: 16px;
}
</style>
