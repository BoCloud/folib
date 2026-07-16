<template>
  <div class="wrapper">
    <!-- 项目漏洞 -->
    <a-card :bordered="false" class="vulnerabilities-card" :bodyStyle="{ padding: 0 }" style="margin-bottom: 20px">
      <a-descriptions v-if="artifactData.layout !== 'Docker'" title="" :column="2"
        style="word-break: break-all;word-wrap: break-word;">
        <a-descriptions-item :label="$t('Artifacts.OwningSpace')">
          {{ artifactData.artifact.storageId }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.OwnedWarehouse')">
          {{ artifactData.artifact.repositoryId }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.ProductPath')">
          {{ artifactData.artifact.artifactPath }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.FileSize')">
          {{ fileSizeConver(artifactData.artifact.sizeInBytes) }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.ModifyTheTime')">
          {{ artifactData.lastModified }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.ServiceTime')">
          {{ artifactData.lastUsedTime }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.ScanTime')">
          {{ artifactData.scanTime }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.DownloadTimes')">
          {{ artifactData.downloadCount }}
        </a-descriptions-item>
      </a-descriptions>
      <a-descriptions v-if="artifactData.layout !== 'Docker'" title="" :column="1"
        style="word-break: break-all;word-wrap: break-word;">
        <template v-if="artifactData && artifactData.artifact && artifactData.artifact.checksums">
          <a-descriptions-item :label="key" v-for="(value, key, index) in artifactData.artifact.checksums" :key="index"
            span="2">
            {{ value }}
          </a-descriptions-item>
        </template>
      </a-descriptions>
      <a-descriptions v-if="artifactData.layout === 'Docker'" title="" :column="2">
        <a-descriptions-item :label="$t('Artifacts.OwningSpace')">
          {{ artifactData.artifact.storageId }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.OwnedWarehouse')">
          {{ artifactData.artifact.repositoryId }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.ImageName')">
          {{ artifactData.artifact.artifactCoordinates.name }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.VersionNumber')">
          {{ artifactData.artifact.artifactCoordinates.version }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.FileSize')">
          {{ fileSizeConver( artifactData.size) }}
        </a-descriptions-item>
        <a-descriptions-item :label="SHA-256">
          {{ artifactData.sha256 }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.ModifyTheTime')">
          {{ artifactData.lastModified }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.ScanTime')">
          {{ artifactData.scanTime }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.NumberOfFloors')">
          {{ artifactData.manifest.layers.length }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.MakeADockerVersion')">
          {{ artifactData.manifestConfig.docker_version }}
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.MirrorOS')">
          <a-tag> {{ artifactData.manifestConfig.os }}</a-tag>
        </a-descriptions-item>
        <a-descriptions-item :label="$t('Artifacts.TheInfrastructure')">
          {{ artifactData.manifestConfig.architecture }}
        </a-descriptions-item>
      </a-descriptions>

      <div class="bar">
        <div class="card-inner">
          <div class="bar-card">
            <div class="callout b-severity-unassigned">
              <div class="text">
                <div class="text-muted">{{ $t('Artifacts.Total') }}</div>
                <strong>{{ artifactData.artifact.vulnerabilitiesCount }}</strong>
              </div>
            </div>
          </div>
          <div class="bar-card">
            <div class="callout b-severity-critical">
              <div class="text">
                <div class="text-muted">{{ $t('Artifacts.Seriously') }}</div>
                <strong>{{ artifactData.artifact.criticalVulnerabilitiesCount }}</strong>
              </div>
            </div>
          </div>
          <div class="bar-card">
            <div class="callout b-severity-high">
              <div class="text">
                <div class="text-muted">{{ $t('Artifacts.HighRisk') }}</div>
                <strong>{{ artifactData.artifact.highVulnerabilitiesCount }}</strong>
              </div>
            </div>
          </div>
          <div class="bar-card">
            <div class="callout b-severity-medium">
              <div class="text">
                <div class="text-muted">{{ $t('Artifacts.MediumRisk') }}</div>
                <strong>{{ artifactData.artifact.mediumVulnerabilitiesCount }}</strong>
              </div>
            </div>
          </div>
          <div class="bar-card">
            <div class="callout b-severity-low">
              <div class="text">
                <div class="text-muted">{{ $t('Artifacts.LowRisk') }}</div>
                <strong>{{ artifactData.artifact.lowVulnerabilitiesCount }}</strong>
              </div>
            </div>
          </div>
        </div>
      </div>
    </a-card>
    <!-- 违反政策 -->
    <a-row :gutter="[24]">
      <a-col :span="12">
        <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 10 }">
          <ComponentGraph :artifactData="artifactData"></ComponentGraph>
        </a-card>
      </a-col>
      <a-col :span="12">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 10 }">
              <ChartArtifactComponent :artifactData="artifactData"/>
            </a-card>
          </a-col>
          <a-col :span="24" class="mt-20">
            <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 10 }">
              <ChartArtifactLicense :artifactData="artifactData"/>
            </a-card>
          </a-col>
        </a-row>
      </a-col>
    </a-row>
  </div>
</template>

<script>
import {
  getFileImage,
  fileSizeConver,
} from "@/utils/layoutUtil";
import ChartPolicyViolations from "./Components/ChartPolicyViolations.vue";
import ChartArtifactLicense from "./Components/ChartArtifactLicense.vue";
import ChartComponentVulnerabilities from "./Components/ChartComponentVulnerabilities.vue";
import ChartArtifactComponent from "./Components/ChartArtifactComponent.vue";
import ComponentGraph from "./Components/ComponentGraph.vue";
export default {
  components: { ChartPolicyViolations, ChartComponentVulnerabilities, ChartArtifactLicense, ChartArtifactComponent, ComponentGraph },
  props: {
    artifactData: {
      type: Object,
      default: {},
    },
  },
  data() {
    return {
    };
  },
  watch: {
    artifactData: {
      handler(newVal, oldVal) {
      },
      deep: true, // 开启深度监听
    },
  },
  mounted() {
  },

  beforeDestroy: function () {
  },

  methods: {
    fileSizeConver(size) {
      if (size) {
        return fileSizeConver(size)
      }
    },
  },
};
</script>

<style lang="scss" scoped>
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
  width: 20%;
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
