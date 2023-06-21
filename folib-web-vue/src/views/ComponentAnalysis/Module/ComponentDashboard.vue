<template>
  <div class="wrapper component-dashboard">
    <!-- 组件总览 -->
    <a-row :gutter="[24]" type="flex">
      <a-col :span="24" :md="16">
        <a-card :bordered="false" class="component-card" :bodyStyle="{ padding: 0 }" style="margin-bottom: 20px">
          <a-descriptions title="" :column="1"
            style="word-break: break-all;word-wrap: break-word;">
            <a-descriptions-item label="组件名称">
              {{ component.name }}
            </a-descriptions-item>
            <a-descriptions-item label="组件版本">
              {{ component.version }}
            </a-descriptions-item>
            <a-descriptions-item label="组件描述">
              {{ component.description }}
            </a-descriptions-item>
            <a-descriptions-item label="License">
              {{ component.license?component.license.join(","):'' }}
            </a-descriptions-item>
            <a-descriptions-item label="PURL">
              {{ component.purl }}
            </a-descriptions-item>
            <a-descriptions-item label="MD5">
              {{ component.md5sum }}
            </a-descriptions-item>
            <a-descriptions-item label="SHA1">
              {{ component.sha1sum }}
            </a-descriptions-item>
            <a-descriptions-item label="SHA256">
              {{ component.sha256sum }}
            </a-descriptions-item>
          </a-descriptions>
          <div class="bar mt-10">
            <div class="card-inner">
              <div class="bar-card">
                <div class="callout b-severity-unassigned">
                  <div class="text">
                    <div class="text-muted">总数</div>
                    <strong>{{ component.vulnerabilitiesCount }}</strong>
                  </div>
                </div>
              </div>
              <div class="bar-card">
                <div class="callout b-severity-critical">
                  <div class="text">
                    <div class="text-muted">严重</div>
                    <strong>{{ component.criticalVulnerabilitiesCount }}</strong>
                  </div>
                </div>
              </div>
              <div class="bar-card">
                <div class="callout b-severity-high">
                  <div class="text">
                    <div class="text-muted">高危</div>
                    <strong>{{ component.highVulnerabilitiesCount }}</strong>
                  </div>
                </div>
              </div>
              <div class="bar-card">
                <div class="callout b-severity-medium">
                  <div class="text">
                    <div class="text-muted">中危</div>
                    <strong>{{ component.mediumVulnerabilitiesCount }}</strong>
                  </div>
                </div>
              </div>
              <div class="bar-card">
                <div class="callout b-severity-low">
                  <div class="text">
                    <div class="text-muted">低危</div>
                    <strong>{{ component.lowVulnerabilitiesCount }}</strong>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </a-card>
      </a-col>
      <a-col :span="24" :md="8">
        <a-row :gutter="24">
          <a-col :span="24" :lg="24" :xl="24" class="mb-24" style="position: relative; z-index: 1;">
            <a-row :gutter="24">
              <a-col :span="12" :md="12">
                <a-card :bordered="false" class="widget-2">
                  <a-statistic :value="artifactStatistics.storageCount">
                    <template #title>
                      <div class="icon">
                        <a-avatar :size="40" shape="square" src="images/folib/storage.svg" />

                        <!-- <a-icon type="appstore" theme="filled" :style="{ fontSize: '28px' }" /> -->
                      </div>
                      <span>关联存储空间</span>
                    </template>
                  </a-statistic>
                </a-card>
              </a-col>
              <a-col :span="12" :md="12">
                <a-card :bordered="false" class="widget-2">
                  <a-statistic :value="artifactStatistics.storageCount">
                    <template #title>
                      <div class="icon">
                        <a-icon type="appstore" theme="filled" :style="{ fontSize: '28px' }" />
                      </div>
                      <span>关联仓库</span>
                    </template>
                  </a-statistic>
                </a-card>
              </a-col>
            </a-row>
          </a-col>
          <a-col :span="24" :lg="24" :xl="24" class="mb-24" style="position: relative; z-index: 1;">
            <a-row :gutter="24">
              <a-col :span="12" :md="12">
                <a-card :bordered="false" class="widget-2">
                  <a-statistic :value="artifactStatistics.artifactCount">
                    <template #title>
                      <div class="icon">
                        <a-icon type="database" theme="filled" :style="{ fontSize: '28px' }" />
                      </div>
                      <span>关联制品</span>
                    </template>
                  </a-statistic>
                </a-card>
              </a-col>
              <a-col :span="12" :md="12">
                <a-card :bordered="false" class="widget-2">
                  <a-statistic :value="component.license?component.license.length:0">
                    <template #title>
                      <div class="icon">
                        <a-icon type="file-text" theme="filled" :style="{ fontSize: '28px' }" />
                      </div>
                      <span>License</span>
                    </template>
                  </a-statistic>
                </a-card>
              </a-col>
            </a-row>
          </a-col>
        </a-row>
      </a-col>
    </a-row>

    <div class="wrapper-com">
      <a-tabs class="tabs-sliding" default-active-key="1" @change="handleChangeTabs">
        <a-tab-pane key="1" tab="图谱">
          <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 10 }">
            <ArtifactGraph :component="component" v-if="tabActive == 1"></ArtifactGraph>
          </a-card>
        </a-tab-pane>
        <a-tab-pane key="2" tab="列表">
          <AffectedArtifacts :component="component" v-if="tabActive == 2"></AffectedArtifacts>
        </a-tab-pane>
      </a-tabs>
    </div>
  </div>
</template>

<script>
import { getArtifactStatistics } from "@/api/module.js";
import { formatTimestamp, valueWithDefault } from "@/utils/util.js";
import ArtifactGraph from "./ArtifactGraph.vue";
import AffectedArtifacts from "./AffectedArtifacts.vue";
export default {
  components: { ArtifactGraph, AffectedArtifacts },
  props: {
    component: {
      type: Object,
      default: {},
    },
  },
  data() {
    return {
      tabActive: 1,
      artifactStatistics: {
        storageCount: 0,
        repositoryCount: 0,
        artifactCount: 0,
      }
    };
  },
  watch: {
    component: {
      handler(newVal, oldVal) {
        this.getArtifactStatistics()
      },
      deep: true, // 开启深度监听
    },
  },
  mounted() {
    this.getArtifactStatistics()
  },
  methods: {
    getArtifactStatistics() {
      if (!this.component.uuid) {
				return
			}
      getArtifactStatistics({ componentUuid: this.component.uuid }).then((res) => {
        this.artifactStatistics = res
      });
    },
    handleChangeTabs(val) {
      this.tabActive = val;
    },
  },
};
</script>

<style lang="scss" scoped>
.component-dashboard::v-deep {
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

  .component-card {
    padding: 24px;
    height: 408px;
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

  .widget-2 .icon svg path {
    fill: #ffffff;
  }

  .widget-2 .icon img{
    width: unset;
  }
}
</style>
