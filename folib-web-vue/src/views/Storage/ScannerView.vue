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
                  <h6>{{ scanCurrentData.countFolib }}</h6>
                </div>
              </div>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="scanCurrentData.denpendencySum">
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
              <a-statistic :value="scanCurrentData.vulnerabilitesSum">
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
              <a-statistic :value="scanCurrentData.vulnerableSum">
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
              <a-statistic :value="scanCurrentData.suppressedSum">
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
                :data-source="rowData" :pagination="false">
                <template slot="path" slot-scope="text, record">
                  <div @click="folibScannerGetOne(record)">
                    <a>
                      <h6 class="m-0">
                        {{ record.path }}
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
                :data-source="rowData" :pagination="false">
                <a-table rowKey="id" :columns="innerColumns" slot="expandedRowRender" slot-scope="record"
                  :data-source="record.childList" :pagination="false">
                  <template slot="path" slot-scope="text, record">
                    <div @click="folibScannerGetOne(record)">
                      <a>
                        <h6 class="m-0">
                          {{ record.path }}
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


    <a-drawer placement="right" width="65%" title="报告详情" :visible="detialVisible" @close="closeDialog">
      <a-collapse default-active-key="1" :bordered="false" accordion>
        <template #expandIcon="props">
          <a-icon type="caret-right" :rotate="props.isActive ? 90 : 0" />
        </template>
        <a-collapse-panel v-for="(item, index) in currentReport" :key="index"
          style='background: #f7f7f7;border-radius: 4px;margin-bottom: 24px;border: 0;overflow: hidden'>
          <template slot="header">
            <div class="collapse-panel-header-info">
              <span class="file-name">{{ item.fileName }}</span>
              <a-tooltip v-if="item.vulnerabilitiesCount > 0">
                <template slot="title">漏洞数量</template>
                <a-avatar :size="24" :src="'images/folib/bug.svg'" />
                <span class="mb-0 text-dark bug-count">{{ item.vulnerabilitiesCount }}</span>
              </a-tooltip>
              <a-tooltip v-else>
                <template slot="title">健康</template>
                <a-avatar :size="24" :src="'images/folib/healthy.svg'" />
              </a-tooltip>
            </div>
          </template>
          <a-card :bordered="false" class="card-order header-solid mb-24 mx-auto mt-20 mb-50"
            :bodyStyle="{ paddingTop: 0 }">
            <template #title>
              <h6 class="mb-0">{{ item.fileName }}</h6>
            </template>
            <a-row :gutter="[24]" type="flex">
              <a-col :span="24" :md="16">
                <p class="mb-0">
                  该依赖含有 <strong>{{ item.evidence.length }}</strong> 个风险凭证，并在扫描检测中发现
                  <strong>{{ item.vulnerabilitiesCount }}</strong>个漏洞
                </p>
                <p class="mb-0">
                  MD5: <strong>{{ item.md5sum }}</strong>
                </p>
                <p class="mb-0">
                  SHA256: <strong>{{ item.sha256sum }}</strong>
                </p>
              </a-col>
              <a-col :span="24" :md="8" class="ml-auto text-right">
                <p class="mb-0">
                  版本号: <strong>{{ item.version }}</strong>
                </p>

              </a-col>
            </a-row>
            <hr class="gradient-line">

            <a-row :gutter="[24]" type="flex" class="order-products" align="middle">
              <a-col :span="24" :md="12">
                <div class="d-flex">
                  <a-avatar class="mr-15" :src="'images/folib/' + getImage(item.ecosystem) + '.svg'" shape="square"
                    :size="80" />
                  <div>
                    <h6 class="mb-0 mt-10 font-semibold">{{ item.name }}</h6>
                    <p class="mb-15">
                      License: <strong>{{ item.license }}</strong>
                    </p>
                    <a-tag class="ant-tag-success font-semibold">{{ item.ecosystem }}</a-tag>
                  </div>
                </div>
              </a-col>
              <a-col :span="24" :md="12" class="ml-auto text-right">
                <p>{{ item.description }}</p>
              </a-col>
            </a-row>

            <hr class="gradient-line">

            <a-row :gutter="[24]" type="flex">
              <a-col :span="24" :md="24" :lg="24">
                <a-table :columns="vulnerColumns" :data-source="item.vulnerabilities" :pagination="false">

                  <a-row slot="expandedRowRender" :gutter="[24, 24]" slot-scope="record">
                    <a-col :span="24">
                      <a-card :bordered="false" class="card-billing-info">
                        <div class="col-info">
                          <a-descriptions :title="record.references.length + '个参考信息'" :column="1">
                            <a-descriptions-item label="说明">
                              以下信息均来自于开源社区
                            </a-descriptions-item>
                            <a-descriptions-item label="相关信息链接">
                              <p v-for="(ritem, index1) in record.references" :key="index1">
                                {{ ritem.url }}
                              </p>

                            </a-descriptions-item>
                          </a-descriptions>
                        </div>
                      </a-card>
                    </a-col>
                  </a-row>
                  <template slot="name" slot-scope="text, record">
                    <div>
                      <a>
                        <h6 class="m-0">
                          {{ record.name }}
                        </h6>
                      </a>
                    </div>
                  </template>
                  <template slot="highestSeverityText" slot-scope="highestSeverityText">
                    <div class="table-avatar-info">
                      <a-avatar v-if="['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(highestSeverityText) != -1" :size="24"
                        :src="'images/folib/' + highestSeverityText.toLowerCase() + '.svg'" />
                      <a-avatar v-else shape="circle" :size="24">{{ highestSeverityText.slice(0, 1) }}</a-avatar>
                      <div class="avatar-info">
                        <p class="mb-0 text-dark">{{
                            highestSeverityText === 'CRITICAL' ? '严重' : highestSeverityText === 'MEDIUM' ? '中危' : highestSeverityText === 'HIGH' ? '高危' : highestSeverityText === 'LOW' ? '低危' : highestSeverityText
                        }}
                        </p>
                      </div>
                    </div>
                  </template>
                  <template slot="v2_exploitabilityScore" slot-scope="text, record">{{ record.cvssV2.score }}</template>
                  <template slot="v3_exploitabilityScore" slot-scope="text, record">{{ record.cvssV3.baseScore
                  }}</template>
                  <template slot="versionStartIncluding" slot-scope="text, record">{{
                      record.matchedVulnerableSoftware.versionStartIncluding
                  }}</template>
                  <template slot="versionEndExcluding" slot-scope="text, record">{{
                      record.matchedVulnerableSoftware.versionEndExcluding
                  }}</template>

                </a-table>
              </a-col>
            </a-row>
          </a-card>
        </a-collapse-panel>
      </a-collapse>
    </a-drawer>
  </div>
</template>

<script>

import { folibScannerGetOne, folibScannerPage, folibScannerDockerPage } from "@/api/folib";
import { getLayoutType2 } from "@/utils/layoutUtil";
import ChartBar from '@/components/Charts/ChartBar';
import ChartLine from '@/components/Charts/ChartLine'
import storage from "store";

export default ({
  components: {
    ChartBar,
    ChartLine,

  },
  created() {
    this.getCountData()
  },
  data() {
    return {
      vulnerColumns: [
        {
          title: 'CVE编号',
          dataIndex: 'name',
          scopedSlots: { customRender: 'name' },
        },
        {
          title: '漏洞等级',
          dataIndex: 'highestSeverityText',
          scopedSlots: { customRender: 'highestSeverityText' },
        },
        {
          title: 'CvssV2评分',
          dataIndex: 'cvssV2',
          scopedSlots: { customRender: 'v2_exploitabilityScore' },
        },
        {
          title: 'CvssV3评分',
          dataIndex: 'cvssV3',
          scopedSlots: { customRender: 'v3_exploitabilityScore' },
        },
        {
          title: '引入版本',
          scopedSlots: { customRender: 'versionStartIncluding' }
        },
        {
          title: '建议修复版本',
          scopedSlots: { customRender: 'versionEndExcluding' }
        }
      ],
      detialVisible: false,
      scanCurrentData: {
        denpendencySum: 531,
        vulnerableSum: 34,
        vulnerabilitesSum: 46,
        suppressedSum: 0,
        storage: "folib-common",
        repository: "aliyun-maven",
        layout: "Maven 2",
        countFolib: 479,
        star: 4
      },
      columns: [
        {
          title: '包路径',
          dataIndex: 'path',
          scopedSlots: { customRender: 'path' },
        },
        {
          title: '漏洞数',
          dataIndex: 'vulnerabilitesCount',
          scopedSlots: { customRender: 'vulnerabilitesCount' },
          width: 100,
        },
        {
          title: '依赖数',
          dataIndex: 'dependencyCount',
          width: 100,
        },
        {
          title: '封存漏洞',
          dataIndex: 'suppressedCount',
          width: 100,
        },

        {
          title: '问题依赖数',
          dataIndex: 'vulnerableCount',
          scopedSlots: { customRender: 'vulnerableCount' },
        },
        {
          title: '扫描时间',
          dataIndex: 'scanTime',
        },
      ],
      dockerColumns: [
        {
          title: '镜像名称',
          dataIndex: 'path',
          scopedSlots: { customRender: 'path' },
        },
        {
          title: '版本号',
          dataIndex: 'version',
          width: 100,
        },
        {
          title: '漏洞数',
          dataIndex: 'vulnerabilitesCount',
          scopedSlots: { customRender: 'vulnerabilitesCount' },
          width: 100,
        },
        {
          title: '依赖数',
          dataIndex: 'dependencyCount',
          width: 100,
        },
        {
          title: '封存漏洞',
          dataIndex: 'suppressedCount',
          width: 100,
        },
        {
          title: '问题依赖数',
          dataIndex: 'vulnerableCount',
          scopedSlots: { customRender: 'vulnerableCount' },
        }
      ],
      innerColumns: [
        {
          title: '包路径',
          dataIndex: 'path',
          scopedSlots: { customRender: 'path' },
        },
        {
          title: '漏洞数',
          dataIndex: 'vulnerabilitesCount',
          scopedSlots: { customRender: 'vulnerabilitesCount' },
          width: 100,
        },
        {
          title: '依赖数',
          dataIndex: 'dependencyCount',
          width: 100,
        },
        {
          title: '封存漏洞',
          dataIndex: 'suppressedCount',
          width: 100,
        },

        {
          title: '问题依赖数',
          dataIndex: 'vulnerableCount',
          scopedSlots: { customRender: 'vulnerableCount' },
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
      currentRow: {},
      currentReport: []
    }
  },
  methods: {
    getCountData() {
      const params = storage.get('scannerView_repository')
      this.scanCurrentData = params.item
      this.query.repository = this.scanCurrentData.repository
      this.query.storage = this.scanCurrentData.storage
      // this.query.vulnerableCount=1
      if (this.scanCurrentData.layout.toUpperCase() === 'docker'.toUpperCase()) {
        this.getDockerList()
      } else {
        this.getList()
      }
    },
    getDockerList() {
      folibScannerDockerPage(this.query).then(res => {
        this.rowData = res.data.rows
        this.total = res.data.total
      })
    },
    getList() {
      folibScannerPage(this.query).then(res => {
        this.rowData = res.data.rows
        this.total = res.data.total
      })
    },
    folibScannerGetOne(row) {
      folibScannerGetOne(row.path).then(res => {
        this.currentRow = row
        this.currentReport = res.data
        this.detialVisible = true
      })
    },
    onShowSizeChange(current, pageSize) {
      this.query.limit = pageSize;
      this.query.page = current
      this.getList()
    },
    closeDialog() {
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