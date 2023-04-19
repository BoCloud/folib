<template>
  <div>
    <a-row  type="flex" :gutter="24">
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card :bordered="false" class="widget-2">
          <a-statistic :value="vulnerabilityStatistics.artifactCount">
            <template #title>
              <div class="icon">
                <a-icon
                  type="appstore"
                  theme="filled"
                  :style="{ fontSize: '28px' }"
                />
              </div>
              <h6>制品</h6>
              <p>制品总数（个）</p>
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card :bordered="false" class="widget-2">
          <a-statistic :value="vulnerabilityStatistics.downloadCount">
            <template #title>
              <div class="icon">
                <a-icon type="cloud-download" :style="{ fontSize: '28px' }" />
              </div>
              <h6>下载</h6>
              <p>下载总数（次）</p>
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card :bordered="false" class="widget-2">
          <a-statistic :value="vulnerabilityStatistics.dependencyCount">
            <template #title>
              <div class="icon">
                <a-icon
                  type="control"
                  theme="filled"
                  :style="{ fontSize: '28px' }"
                />
              </div>
              <h6>依赖</h6>
              <p>依赖总数</p>
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card :bordered="false" class="widget-2">
          <a-statistic :value="vulnerabilityStatistics.vulnerabilityCount">
            <template #title>
              <div class="icon">
                <a-icon
                  type="bug"
                  theme="filled"
                  :style="{ fontSize: '28px' }"
                />
              </div>
              <h6>漏洞</h6>
              <p>漏洞总数</p>
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card
          :bordered="false"
          class="widget-2 vulnerability-count"
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
              <h6>白名单</h6>
              <p>漏洞白名单</p>
            </template>
          </a-statistic>
        </a-card>
      </a-col>
      <a-col :span="24" :xl="4" class="mb-24 statistics">
        <a-card
          :bordered="false"
          class="widget-2 vulnerability-count"
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
              <h6>黑名单</h6>
              <p>漏洞黑名单</p>
            </template>
          </a-statistic>
        </a-card>
      </a-col>
    </a-row>
    <a-card>
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
    </a-card>

    
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

export default {
  props: ["folibRepository","vulnerabilityColumns"],
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
        this.vulnerabilityDrawerTitle = "白名单"
        this.vulnerabilityDrawerData = this.vulnerabilityStatistics.whites
      }
      if (type === 2) {
        this.vulnerabilityDrawerTitle = "黑名单"
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