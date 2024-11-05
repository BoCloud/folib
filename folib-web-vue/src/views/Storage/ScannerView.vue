<!--
	This is the Dashboards default page, it uses the dashboard layout in:
	"./layouts/Dashboard.vue" .
 -->

<template>
  <div class="dashboard">

    <a-row :gutter="24" type="flex" align="stretch">
      <a-col :span="24" :lg="24">
        <a-row :gutter="24">
          <a-col :span="12" :xl="8" class="mb-24">
            <a-card class="card-credit  header-solid h-full" style="background-image: url(images/info-card-3.jpg)">
              <template #title>
                <a-avatar :size="42" shape="square" :src="'images/folib/' + LayoutTypeBuild() + '.svg'" class="mr-10">
                </a-avatar>
              </template>
              <h5 class="card-number">{{ scanCurrentData.repository }}</h5>
              <div class="card-footer">
                <div class="mr-30">
                  <p>所属空间</p>
                  <h6>{{ scanCurrentData.storage }}</h6>
                </div>
                <div class="mr-30">
                  <p>安全评分</p>
                  <div class="rating">
                    <a-icon type="star" v-for="n in scanCurrentData.star" :key="n" theme="filled" />
                    <a-icon type="star" v-for="n in (5 - scanCurrentData.star)" :key="6 - n" />
                  </div>
                </div>
                <div class="mr-30">
                  <p>包总数</p>
                  <h6>{{ scanCurrentData.scanCount }}</h6>
                </div>
              </div>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="scanCurrentData.dependencyCount">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/denpendencySum.svg" alt="">
                  </div>
                  <h6>扫描依赖数量</h6>
                  <p>依赖指的是包底层依赖</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="scanCurrentData.vulnerabilitiesCount">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/vulnerabilitesSum.svg" alt="">
                  </div>
                  <h6>依赖漏洞数</h6>
                  <p>指包底层依赖有一定的风险</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="scanCurrentData.dependencyVulnerabilitiesCount">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/vulnerableSum.svg" alt="">
                  </div>
                  <h6>有漏洞的包数量</h6>
                  <p>底层依赖有漏洞的包数量之和</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="scanCurrentData.suppressedVulnerabilitiesCount">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/suppressedSum.svg" alt="">
                  </div>
                  <h6>被封锁的漏洞数量</h6>
                  <p>指存在漏洞但是已经被封锁</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
        </a-row>
        <a-row :gutter="24">
          <a-col :span="24" :lg="24" class="mb-24" style="position: relative; z-index: 1;">
            <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 0, }">
              <template #title>
                <a-row type="flex" align="middle">
                  <a-col :span="24" :md="12">
                    <h6>仓库扫描情况</h6>
                  </a-col>
                  <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                    <a-pagination v-model="query.page" :total="total" :show-total="total => `共 ${total} 个`"
                      :page-size="query.limit" @change="onShowSizeChange" size="small">
                    </a-pagination>
                  </a-col>
                </a-row>
              </template>
              <div class="mx-25 search">
                <a-col :span="24" class="text-right">
                  <a-input-search placeholder="输入关键词查询" class="s-search"
                    v-model="query.artifactName" @search="getCountData()" />
                </a-col>
              </div>
              <a-table v-if="scanCurrentData.layout.toUpperCase() != 'docker'.toUpperCase()" :columns="columns"
                :data-source="rowData" :scroll="{ x: true }" :pagination="false" :row-key="(r, i) => i.toString()">
                <template slot="filePath" slot-scope="text, record">
                  <div @click="getScanReport(record)">
                    <a>
                      <h6 class="m-0">
                        {{ record.filePath }}
                      </h6>
                    </a>
                  </div>
                </template>

                <template slot="star" slot-scope="star">
                  <div class="rating">
                    <a-icon type="star" v-for="n in star" :key="n" theme="filled" />
                    <a-icon type="star" v-for="n in (5 - star)" :key="6 - n" />
                  </div>
                </template>
              </a-table>
              <a-table v-if="scanCurrentData.layout.toUpperCase() == 'docker'.toUpperCase()" :columns="dockerColumns"
                :data-source="rowData" :scroll="{ x: true }" :pagination="false" :row-key="(r, i) => i.toString()">
                <a-table rowKey="id" :columns="innerColumns" slot="expandedRowRender" slot-scope="record" :scroll="{ x: true }"
                  :data-source="record.filePaths" :pagination="false">
                  <template slot="filePath" slot-scope="text, item">
                    <div @click="getScanReport(record)">
                      <a>
                        <h6 class="m-0">
                          {{ item.filePath }}
                        </h6>
                      </a>
                    </div>
                  </template>

                  <template slot="star" slot-scope="star">
                    <div class="rating">
                      <a-icon type="star" v-for="n in star" :key="n" theme="filled" />
                      <a-icon type="star" v-for="n in (5 - star)" :key="6 - n" />
                    </div>
                  </template>
                </a-table>
              </a-table>
            </a-card>
          </a-col>
        </a-row>
      </a-col>
    </a-row>
    <VunlerabilityReport :report="scanReport" :reportVisible="detialVisible" @closeReport="closeReport"/>
  </div>
</template>

<script>

import { scannerRepositoryPage, getArtifact, getArtifactReport } from "@/api/folib";
import { getLayoutType2 } from "@/utils/layoutUtil";
import ChartBar from '@/components/Charts/ChartBar';
import ChartLine from '@/components/Charts/ChartLine'
import storage from "store";
import VunlerabilityReport from '@/components/Vulnerabilities/VunlerabilityReport'

export default ({
  components: {
    ChartBar,
    ChartLine,
    VunlerabilityReport
  },
  created() {
    this.getCountData()
  },
  data() {
    return {
      detialVisible: false,
      scanCurrentData: {
        storage:"",
        repository:"",
        layout:"",
        star:1,
        scanCount:0,
        dependencyCount: 0,
        dependencyVulnerabilitiesCount:0,
        vulnerabilitiesCount:0,
        suppressedVulnerabilitiesCount:0
      },
      columns: [
        {
          title: '包路径',
          dataIndex: 'filePath',
          scopedSlots: { customRender: 'filePath' },
        },
        {
          title: '漏洞数',
          dataIndex: 'vulnerabilitiesCount',
          scopedSlots: { customRender: 'vulnerabilitiesCount' },
          width: 100,
        },
        {
          title: '依赖数',
          dataIndex: 'dependencyCount',
          width: 100,
        },
        {
          title: '封存漏洞',
          dataIndex: 'suppressedVulnerabilitiesCount',
          width: 100,
        },

        {
          title: '问题依赖数',
          dataIndex: 'dependencyVulnerabilitiesCount',
          scopedSlots: { customRender: 'dependencyVulnerabilitiesCount' },
        },
        {
          title: '扫描时间',
          dataIndex: 'scanTime',
        },
      ],
      dockerColumns: [
        {
          title: '镜像名称',
          dataIndex: 'imageName',
          scopedSlots: { customRender: 'imageName' },
        },
        {
          title: '版本号',
          dataIndex: 'version',
          width: 100,
        },
        {
          title: '漏洞数',
          dataIndex: 'vulnerabilitiesCount',
          scopedSlots: { customRender: 'vulnerabilitiesCount' },
          width: 100,
        },
        {
          title: '依赖数',
          dataIndex: 'dependencyCount',
          width: 100,
        },
        {
          title: '封存漏洞',
          dataIndex: 'suppressedVulnerabilitiesCount',
          width: 100,
        },
        {
          title: '问题依赖数',
          dataIndex: 'dependencyVulnerabilitiesCount',
          scopedSlots: { customRender: 'dependencyVulnerabilitiesCount' },
        }
      ],
      innerColumns: [
        {
          title: '包路径',
          dataIndex: 'filePath',
          scopedSlots: { customRender: 'filePath' },
        },
        {
          title: '漏洞数',
          dataIndex: 'vulnerabilitiesCount',
          scopedSlots: { customRender: 'vulnerabilitiesCount' },
          width: 100,
        },
        {
          title: '依赖数',
          dataIndex: 'dependencyCount',
          width: 100,
        },
        {
          title: '封存漏洞',
          dataIndex: 'suppressedVulnerabilitiesCount',
          width: 100,
        },

        {
          title: '问题依赖数',
          dataIndex: 'dependencyVulnerabilitiesCount',
          scopedSlots: { customRender: 'dependencyVulnerabilitiesCount' },
        },
        {
          title: '扫描时间',
          dataIndex: 'scanTime',
        },
      ],
      query: {
        page: 1,
        limit: 10,
        repository: undefined,
        storage: undefined,
        artifactName: undefined,
      },
      total: 50,
      rowData: [],
      scanReport: []
    }
  },
  methods: {
    getCountData() {
      const params = storage.get('scannerView_repository')
      this.scanCurrentData = params.item
      this.query.repository = this.scanCurrentData.repository
      this.query.storage = this.scanCurrentData.storage
      this.getList()
    },
    getList() {
      scannerRepositoryPage(this.query).then(res => {
        this.rowData = res.list
        this.total = res.total
      })
    },
    getScanReport(row) {
      getArtifactReport(
        null,
        row.storageId,
        row.repositoryId,
        row.artifactPath
      ).then(res => {
        let artifact = res.artifact
        this.scanReport = []
        if (artifact && artifact.safeLevel === "scanComplete") {
          this.scanReport = JSON.parse(artifact.report)
          this.detialVisible = true
        }
      })
    },
    onShowSizeChange(current, pageSize) {
      this.query.limit = pageSize;
      this.query.page = current
      this.getList()
    },
    closeReport() {
      this.detialVisible = false
    },
    LayoutTypeBuild() {
      return getLayoutType2(this.scanCurrentData.layout, this.scanCurrentData.repository, 'black')
    },
    getImage(ecosystem) {
      return ecosystem ? ecosystem : this.LayoutTypeBuild()
    }
  }
})

</script>

<style lang="scss" scoped>
$md: 768px;

.dashboard::v-deep {
  .globeContainer {
    position: absolute;
    top: 0;
    right: 0;
    margin-top: 15rem;
    margin-right: 6rem;
  }

  .ant-row-flex {
    position: relative;
    z-index: 1;

    @media(min-width: $md) {
      position: static;
    }
  }
	.search {
		height: 50px;
	}
  
  .s-search{
    max-width: 200px;
		width: 170px;
		min-width: 150px;
  }
}

.collapse-panel-header-info {
  display: inline-block;
}

.collapse-panel-header-info .file-name,
.bug-count {
  margin-right: 10px;
}

.collapse-panel-header-info .bug-count {
  vertical-align: middle;
  margin-left: 2.5px;
}
</style>