<template>
  <div :style="isChecked ? 'padding-left: 3px;padding-right: 3px;height:calc(100vh - 270px);overflow-y: auto;overflow-x: hidden;':''">
    <a-row  type="flex" :gutter="24" style="padding-top: 10px;padding-bottom: 10px">
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card :bordered="false" class="widget-2 card-shadow">
          <a-statistic :value="vulnerabilityStatistics.artifactCount">
            <template #title>
              <div class="icon">
                <a-icon
                  type="appstore"
                  theme="filled"
                  :style="{ fontSize: '28px' }"
                />
              </div>
              <h6>{{ $t('Safe.Products') }}</h6>
              <textOver
                  :text="$t('Safe.ProductsNum')"
                  :max="20"
              />
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card :bordered="false" class="widget-2 card-shadow">
          <a-statistic :value="vulnerabilityStatistics.downloadCount">
            <template #title>
              <div class="icon">
                <a-icon type="cloud-download" :style="{ fontSize: '28px' }" />
              </div>
              <h6>{{ $t('Safe.Download') }}</h6>
              <textOver
                  :text="$t('Safe.DownloadsNum')"
                  :max="20"
              />
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card :bordered="false" class="widget-2 card-shadow">
          <a-statistic :value="vulnerabilityStatistics.dependencyCount">
            <template #title>
              <div class="icon">
                <a-icon
                  type="control"
                  theme="filled"
                  :style="{ fontSize: '28px' }"
                />
              </div>
              <h6>{{ $t('Safe.Dependency') }}</h6>
              <textOver
                  :text="$t('Safe.DependenciesNum')"
                  :max="20"
              />
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card :bordered="false" class="widget-2 card-shadow">
          <a-statistic :value="vulnerabilityStatistics.vulnerabilityCount">
            <template #title>
              <div class="icon">
                <a-icon
                  type="bug"
                  theme="filled"
                  :style="{ fontSize: '28px' }"
                />
              </div>
              <h6>{{ $t('Safe.Bug') }}</h6>
              <textOver
                  :text="$t('Safe.BugNum')"
                  :max="20"
              />
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card
          :bordered="false"
          class="widget-2 card-shadow vulnerability-count"
          @click="vulnerabilityDrawerShow(1)"
        >
          <a-statistic
            :value="vulnerabilityStatistics.whites.length"
            :value-style="{
              color: 'green',
              'text-decoration': 'underline',
            }"
          >
            <template #title>
              <div class="icon">
                <a-icon type="file-done" :style="{ fontSize: '28px' }" />
              </div>
              <h6>{{ $t('Safe.WhiteList') }}</h6>
              <textOver
                  :text="$t('Safe.bugWhitelist')"
                  :max="20"
              />
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card
          :bordered="false"
          class="widget-2 card-shadow vulnerability-count"
          @click="vulnerabilityDrawerShow(2)"
        >
          <a-statistic
            :value="vulnerabilityStatistics.blacks.length"
            :value-style="{
              color: '#cf1322',
              'text-decoration': 'underline',
            }"
          >
            <template #title>
              <div class="icon">
                <a-icon type="exception" :style="{ fontSize: '28px' }" />
              </div>
              <h6>{{ $t('Safe.Blacklist') }}</h6>
              <textOver
                  :text="$t('Safe.bugBlacklist')"
                  :max="20"
              />
            </template>
          </a-statistic>
        </a-card>
      </a-col>
    </a-row>
    <Vulnerability
      :vulnerabilityColumns="vulnerabilityColumns"
      :queryStorageId="false"
      :vulnerabilityLevel="2"
      :queryRepositoryId="false"
      :storageId="folibRepository.storageId"
      :repositoryId="folibRepository.id"
      :repositoryType="folibRepository.type"
      @repositoryVulnerabilityStatistics="repositoryVulnerabilityStatistics"
      ref="vulnerability"
    >
    </Vulnerability>


    <a-drawer
      placement="right"
      width="20%"
      :title="vulnerabilityDrawerTitle"
      :visible="vulnerabilityDrawerVisible"
      @close="vulnerabilityDrawerClose()"
    >
      <!-- <a-card class="header-solid"> -->
      <a-list
        item-layout="vertical"
        size="large"
        :data-source="vulnerabilityDrawerData"
        :pagination="
          vulnerabilityDrawerData.length === 0
            ? false
            : {
                pageSize: 10,
                total: vulnerabilityDrawerData.length,
                showLessItems: true,
              }
        "
      >
        <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
          {{ item }}
        </a-list-item>
      </a-list>
      <!-- </a-card> -->
    </a-drawer>

  </div>
</template>
<script>
import Vulnerability from "@/components/Vulnerabilities/Vulnerability";
import { repositoryVulnerabilityStatistics,  getRepositoryResponseEntity} from '@/api/folib'
import TextOver from "@/components/Tools/textOver";

export default {
  props: ["folibRepository","vulnerabilityColumns",'isChecked'],
  data() {
    return {
      vulnerabilityStatistics: {
        artifactCount: 0,
        downloadCount: 0,
        dependencyCount: 0,
        vulnerabilityCount: 0,
        whites: [],
        blacks: [],
      },
      vulnerabilityDrawerVisible: false,
      vulnerabilityDrawerTitle: "",
      vulnerabilityDrawerData: [],
    };
  },
  components: {
    TextOver,
    Vulnerability,
  },
  created() {
    this.repositoryVulnerabilityStatistics();
  },
  mounted() {},
  methods: {
    repositoryVulnerabilityStatistics() {
      repositoryVulnerabilityStatistics({
        storageId: this.folibRepository.storageId,
        repositoryId: this.folibRepository.id,
      }).then((res) => {
        this.vulnerabilityStatistics = res
      });
    },
    vulnerabilityDrawerShow(type) {
      if (type === 1) {
        this.vulnerabilityDrawerTitle = this.$t('Safe.WhiteList')
        this.vulnerabilityDrawerData = this.vulnerabilityStatistics.whites
      }
      if (type === 2) {
        this.vulnerabilityDrawerTitle = this.$t('Safe.Blacklist')
        this.vulnerabilityDrawerData = this.vulnerabilityStatistics.blacks
      }
      this.vulnerabilityDrawerVisible = true
    },
    vulnerabilityDrawerClose() {
      this.vulnerabilityDrawerVisible = false
    },
  },
};
</script>
<style lang="scss">
.statistics {
  .card-shadow.ant-card {
      box-shadow: -1px 2px 15px -6px rgba(66,73,97,0.53)!important;
  }
}
</style>