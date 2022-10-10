<!--
	This is the Dashboards default page, it uses the dashboard layout in:
	"./layouts/Dashboard.vue" .
 -->

<template>
  <div class="dashboard">
    <a-row :gutter="24" type="flex" align="stretch">
      <a-col :span="24" :lg="24">
        <a-row :gutter="24">
          <a-col :span="24" :lg="24" :xl="6" class="mb-24" style="position: relative; z-index: 1;">
            <!-- Widget 1 Card -->
            <a-card :bordered="false" class="widget-1">
              <a-statistic
                  title="扫描包总数"
                  :value="countData.totalCount.onScanCount"
                  :suffix="onScanProportion.toFixed(2)+'%'"
                  class="text-success"
              >
              </a-statistic>
              <div class="icon">
                <a-icon type="clock-circle" theme="filled" />
              </div>
            </a-card>
            <!-- / Widget 1 Card -->
          </a-col>
          <a-col :span="24" :lg="24" :xl="6" class="mb-24" style="position: relative; z-index: 1;">
            <!-- Widget 1 Card -->
            <a-card :bordered="false" class="widget-1">
              <a-statistic
                  title="无需扫描包总数"
                  :value="countData.totalCount.notScanCount"
                  class="text-success"
              >
              </a-statistic>
              <div class="icon">
                <a-icon type="stop" theme="filled" />
              </div>
            </a-card>
            <!-- / Widget 1 Card -->
          </a-col>
          <a-col :span="24" :lg="24" :xl="6" class="mb-24" style="position: relative; z-index: 1;">
            <!-- Widget 1 Card -->
            <a-card :bordered="false" class="widget-1">
              <a-statistic
                  title="扫描成功的包数量"
                  :value="countData.totalCount.onScanAndScaned"
                  :suffix="onScanAndScanedProportion.toFixed(2)+'%'"
                  class="text-success"
              >
              </a-statistic>
              <div class="icon">
                <a-icon type="safety-certificate" theme="filled" />
              </div>
            </a-card>
            <!-- / Widget 1 Card -->
          </a-col>
          <a-col :span="24" :lg="24" :xl="6" class="mb-24" style="position: relative; z-index: 1;">
            <!-- Widget 1 Card -->
            <a-card :bordered="false" class="widget-1">
              <a-statistic
                  title="扫描失败的包数量"
                  :value="countData.totalCount.onScanAndScanFailed"
                  class="text-success"
              >
              </a-statistic>
              <div class="icon">
                <a-icon type="alert" theme="filled" />
              </div>
            </a-card>
            <!-- / Widget 1 Card -->
          </a-col>
        </a-row>
        <a-row :gutter="24">
          <a-col :span="24" :lg="24" :xl="6" class="mb-24" style="position: relative; z-index: 1;">
            <!-- Widget 1 Card -->
            <a-card :bordered="false" class="widget-1">
              <a-statistic
                  title="扫描依赖数量"
                  :value="countData.denpendencyCount.denpendencySum"
                  class="text-success"
              >
              </a-statistic>
              <div class="icon">
                <a-icon type="control" theme="filled" />
              </div>
            </a-card>
            <!-- / Widget 1 Card -->
          </a-col>
          <a-col :span="24" :lg="24" :xl="6" class="mb-24" style="position: relative; z-index: 1;">
            <!-- Widget 1 Card -->
            <a-card :bordered="false" class="widget-1">
              <a-statistic
                  title="具有漏洞的包数量"
                  :value="countData.denpendencyCount.vulnerableSum"
                  :suffix="vulnerableSumProportion.toFixed(2)+'%'"
                  class="text-danger"
              >
              </a-statistic>
              <div class="icon">
                <a-icon  type="fire" theme="filled" />
              </div>
            </a-card>
            <!-- / Widget 1 Card -->
          </a-col>
          <a-col :span="24" :lg="24" :xl="6" class="mb-24" style="position: relative; z-index: 1;">
            <!-- Widget 1 Card -->
            <a-card :bordered="false" class="widget-1">
              <a-statistic
                  title="漏洞总数"
                  :value="countData.denpendencyCount.vulnerabilitesSum"
                  :suffix="vulnerabilitesSumProportion.toFixed(2)+'%'"
                  class="text-danger"
              >
              </a-statistic>
              <div class="icon">
                <a-icon type="bug" theme="filled" />
              </div>
            </a-card>
            <!-- / Widget 1 Card -->
          </a-col>
          <a-col :span="24" :lg="24" :xl="6" class="mb-24" style="position: relative; z-index: 1;">
            <!-- Widget 1 Card -->
            <a-card :bordered="false" class="widget-1">
              <a-statistic
                  title="封存漏洞数量"
                  :value="countData.denpendencyCount.suppressedSum"
                  class="text-success"
              >
              </a-statistic>
              <div class="icon">
                <a-icon type="security-scan" theme="filled" />
              </div>
            </a-card>
            <!-- / Widget 1 Card -->
          </a-col>
        </a-row>
        <a-row :gutter="24">
          <a-col :span="24" :lg="24" class="mb-24" style="position: relative; z-index: 1;">
            <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{padding: 0,}">
              <template #title>
                <a-tabs class="tabs-sliding" default-active-key="1">
                  <a-tab-pane key="1" tab="仓库扫描情况">
                    <a-table :columns="columns" :data-source="folibScanData" :pagination="false">
                      <template slot="repository" slot-scope="text, record" >
                        <div @click="goToDetial(record)">
                          <a>
                            <h6 class="m-0">
                              <a-avatar :size="42" shape="square" :src="'images/folib/' + LayoutTypeBuild(record) + '.svg'" style="border-radius: 8px; background-image: linear-gradient( 310deg, #f6f5f5, #e2e2e3 );" class="mr-10"></a-avatar>
                              {{ record.repository }}
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
                  </a-tab-pane>
                  <a-tab-pane key="2" tab="平台漏洞情况">
                    <a-table :columns="vulnerabilityColumns" :data-source="vulnerabilityData" 
                    @change="handleVulnerabilityTableChange"
                    :pagination="{pageSize: vulnerabilityQuery.limit,current:vulnerabilityQuery.page,total:vulnerabilityQuery.total,showLessItems:true}">
                      <template slot="cvssV2Severity" slot-scope="cvssV2Severity">
                        <div class="table-avatar-info" v-if="cvssV2Severity">
                          <a-avatar v-if="['CRITICAL','MEDIUM','HIGH','LOW'].indexOf(cvssV2Severity)!=-1" :size="24" :src="'images/folib/'+cvssV2Severity.toLowerCase()+'.svg'" />
                          <a-avatar v-else shape="circle" :size="24">{{ cvssV2Severity.slice(0, 1) }}</a-avatar>
                          <div class="avatar-info">
                            <p class="mb-0 text-dark">{{ cvssV2Severity==='CRITICAL'?'严重':cvssV2Severity==='MEDIUM'?'中危':cvssV2Severity==='HIGH'?'高危':cvssV2Severity==='LOW'?'低危':cvssV2Severity}}</p>
                          </div>
                        </div>
                      </template>
                      <template slot="cvssV3Severity" slot-scope="cvssV3Severity">
                        <div class="table-avatar-info" v-if="cvssV3Severity">
                          <a-avatar v-if="['CRITICAL','MEDIUM','HIGH','LOW'].indexOf(cvssV3Severity)!=-1" :size="24" :src="'images/folib/'+cvssV3Severity.toLowerCase()+'.svg'" />
                          <a-avatar v-else shape="circle" :size="24">{{ cvssV3Severity.slice(0, 1) }}</a-avatar>
                          <div class="avatar-info">
                            <p class="mb-0 text-dark">{{ cvssV3Severity==='CRITICAL'?'严重':cvssV3Severity==='MEDIUM'?'中危':cvssV3Severity==='HIGH'?'高危':cvssV3Severity==='LOW'?'低危':cvssV3Severity}}</p>
                          </div>
                        </div>
                      </template>
                      <template slot="expandedRowRender" slot-scope="record">
                        <a-tag color="#87d068" class="description-title">漏洞描述</a-tag>
                        <a-textarea class="description" :autoSize="true" :read-only="true" v-model="record.description" />
                      </template>
                      <template slot="highestSeverityText" slot-scope="highestSeverityText">
                        <div class="table-avatar-info">
                          <a-avatar v-if="['CRITICAL','MEDIUM','HIGH','LOW'].indexOf(highestSeverityText)!=-1" :size="24" :src="'images/folib/'+highestSeverityText.toLowerCase()+'.svg'" />
                          <a-avatar v-else shape="circle" :size="24">{{ highestSeverityText.slice(0, 1) }}</a-avatar>
                          <div class="avatar-info">
                            <p class="mb-0 text-dark">{{ highestSeverityText==='CRITICAL'?'严重':highestSeverityText==='MEDIUM'?'中危':highestSeverityText==='HIGH'?'高危':highestSeverityText==='LOW'?'低危':highestSeverityText}}</p>
                          </div>
                        </div>
                      </template>
                      <template slot="operation" slot-scope="text, record">
                        <a-button type="primary" class="v-btn down-excel" @click="downExcel(record)">下载Excel</a-button>
                        <a-button type="default" class="v-btn show-graph" @click="showGraph(record)">查看图谱</a-button>
                        <!-- <a-button type="danger">黑白名单</a-button> -->
                        <!-- <a-button type="ghost" class="px-25">2</a-button> -->
                        <!-- <a-button type="link" class="px-25">4</a-button> -->
                        <!-- <a-button type="text" class="px-25">5</a-button> -->
                        <!-- <a-button type="default" class="px-25">6</a-button> -->
                      </template>
                    </a-table>
                  </a-tab-pane>
                </a-tabs>
              </template>
            </a-card>
          </a-col>
        </a-row>
        <!-- / Sales By Country Table -->

      </a-col>
    </a-row>


    <!-- Charts -->
    <a-row :gutter="24" type="flex" align="stretch">
      <a-col :span="24" :lg="10" class="mb-24">

        <!-- Active Users Card -->
        <a-card :bordered="false" class="dashboard-bar-chart">
          <chart-bar ref="volFolib" :height="220" :data="barChartData"></chart-bar>
          <div class="card-title">
            <h6>近7天漏洞分布视图</h6>
            <p>较上周包数量 <span class="text-success">{{weekCompare.countFolib>0?'+'+weekCompare.countFolib:weekCompare.countFolib===0?'不变':'未知'}}</span></p>
          </div>
          <div class="card-content">
            <p>以下为本周与上周(14天)的数据进行比较的结果</p>
          </div>
          <a-row class="card-footer" type="flex" justify="center" align="top">
            <a-col :span="6">
              <h6>{{weekCompare.vulnerableSum}}</h6>
              <span>漏洞包新增</span>
            </a-col>
            <a-col :span="6">
              <h6>{{weekCompare.denpendencySum}}</h6>
              <span>扫描依赖新增</span>
            </a-col>
            <a-col :span="6">
              <h6>{{weekCompare.vulnerabilitesSum}}</h6>
              <span>漏洞依赖新增</span>
            </a-col>
            <a-col :span="6">
              <h6>{{weekCompare.suppressedSum}}</h6>
              <span>封存漏洞数新增</span>
            </a-col>
          </a-row>
        </a-card>
        <!-- Active Users Card -->

      </a-col>
      <a-col :span="24" :lg="14" class="mb-24">

        <a-card :bordered="false" class="dashboard-bar-line header-solid">
          <template #title>
            <h6>近30天数据</h6>
            <p>本图为近30天数据,无新增数据的日期不展示</p>
          </template>
          <template #extra>
            <a-badge color="primary" class="badge-dot-primary" text="依赖数量" />
            <a-badge color="primary" class="badge-dot-secondary" text="漏洞数量" />
          </template>
          <chart-line  ref="d30map" :height="310" :data="lineChartData"></chart-line>
        </a-card>

      </a-col>
    </a-row>
    <!-- / Charts -->

  </div>
</template>

<script>

import {getCount,getScannerSumDifVoList,vulnerabilityPage,vulnerabilityExportExcel,weekDayCount,mounthDayCount} from "@/api/folib";
import {getLayoutType2} from "@/utils/layoutUtil";
import ChartBar from '@/components/Charts/ChartBar' ;
import ChartLine from '@/components/Charts/ChartLine'
import storage from "store";

export default ({
  components: {
    ChartBar,
    ChartLine,

  },
  created() {
    this.getCountData()
    this.getVulnerabilityPage()
  },
  data() {
    return {
      lineChartData: {
        labels: [],
        datasets: [{
          label: "依赖数量",
          tension: 0.4,
          pointRadius: 0,
          borderColor: "#1890FF",
          borderWidth: 1,
          data: [],
          maxBarThickness: 6

        },
          {
            label: "漏洞数量",
            tension: 0.4,
            pointRadius: 0,
            borderColor: "#B37FEB",
            borderWidth: 1,
            data: [],
            maxBarThickness: 6

          }],
      },
      barChartData: {
        labels: [],
        datasets: [{
          label: "漏洞数量",
          backgroundColor: '#fff',
          borderWidth: 0,
          borderSkipped: false,
          borderRadius: 6,
          data: [],
          maxBarThickness: 20,
        }, ],
      },
      countData: {
        denpendencyCount: {denpendencySum: 0, vulnerableSum: 0, vulnerabilitesSum: 0, suppressedSum: 0},
        totalCount: {onScanCount: 0, onScanAndUnScan: 0, onScanAndScanFailed: 0, notScanCount: 0,onScanAndScaned:0}
      },
      columns: [
        {
          title: '仓库',
          dataIndex: 'repository',
          scopedSlots: { customRender: 'repository' },
        },
        {
          title: '存储空间',
          dataIndex: 'storage',
          scopedSlots: { customRender: 'storage' },
        },
        {
          title: '包总数',
          dataIndex: 'countFolib',
          width: 100,
        },
        {
          title: '问题包数',
          dataIndex: 'vulnerableSum',
          width: 100,
        },
        {
          title: '漏洞数量',
          dataIndex: 'vulnerabilitesSum',
          width: 100,
        },
        {
          title: '封存漏洞数量',
          dataIndex: 'suppressedSum',
        },
        {
          title: '安全评分',
          dataIndex: 'star',
          scopedSlots: { customRender: 'star' },
        }
      ],
      vulnerabilityColumns: [
        {
          title: '漏洞编号',
          dataIndex: 'uuid',
          scopedSlots: { customRender: 'uuid' },
        },
        {
          title: '创建时间',
          dataIndex: 'created',
          scopedSlots: { customRender: 'created' },
        },
        // {
        //   title: '最后更新时间',
        //   dataIndex: 'lastUpdated',
        //   scopedSlots: { customRender: 'lastUpdated' },
        // },
        {
          title: 'CvssV2评分',
          dataIndex: 'cvssV2Score',
          scopedSlots: { customRender: 'cvssV2Score' },
        },
        {
          title: 'CvssV2漏洞等级',
          dataIndex: 'cvssV2Severity',
          scopedSlots: { customRender: 'cvssV2Severity' },
        },
        {
          title: 'CvssV3评分',
          dataIndex: 'cvssV3Score',
          scopedSlots: { customRender: 'cvssV3Score' },
        },
        {
          title: 'CvssV3漏洞等级',
          dataIndex: 'cvssV3Severity',
          scopedSlots: { customRender: 'cvssV3Severity' },
        },
        // {
        //   title: '漏洞描述',
        //   dataIndex: 'description',
        //   scopedSlots: { customRender: 'description' },
        //   ellipsis: true,
        // },
        {
          title: '最高漏洞等级',
          dataIndex: 'highestSeverityText',
          scopedSlots: { customRender: 'highestSeverityText' },
        },
        {
          title: '建议修复版本',
          dataIndex: 'versionEndExcluding',
          scopedSlots: { customRender: 'versionEndExcluding' },
        },
        {
          title: '操作',
          dataIndex: 'operation',
          scopedSlots: { customRender: 'operation' },

        },
      ],
      folibScanData:[],
      vulnerabilityData:[],
      vulnerabilityQuery:{
        page:1,
        limit:10,
        total: 0,
      },
      weekCompare:{},
      onScanProportion: 0.00,
      onScanAndScanedProportion: 0.00,
      vulnerableSumProportion: 0.00,
      vulnerabilitesSumProportion: 0.00,
    }
  },
  methods: {
    getCountData() {
      getCount().then(res => {
        this.countData = res.data
        if(this.countData.totalCount.onScanCount+this.countData.totalCount.notScanCount > 0){
           this.onScanProportion = ((this.countData.totalCount.onScanCount/(this.countData.totalCount.onScanCount+this.countData.totalCount.notScanCount))*100)
        }
        if(this.countData.totalCount.onScanAndScaned+this.countData.totalCount.onScanAndUnScan+this.countData.totalCount.onScanAndScanFailed > 0){
           this.onScanAndScanedProportion = ((this.countData.totalCount.onScanAndScaned/(this.countData.totalCount.onScanAndScaned+this.countData.totalCount.onScanAndUnScan+this.countData.totalCount.onScanAndScanFailed))*100)
        }
        if(this.countData.totalCount.onScanCount > 0){
           this.vulnerableSumProportion = ((this.countData.denpendencyCount.vulnerableSum/(this.countData.totalCount.onScanCount))*100)
        }
        if(this.countData.denpendencyCount.denpendencySum > 0){
           this.vulnerabilitesSumProportion = ((this.countData.denpendencyCount.vulnerabilitesSum/(this.countData.denpendencyCount.denpendencySum))*100)
        }
      })
      getScannerSumDifVoList().then(res =>{
        this.folibScanData=res.data

      })
      weekDayCount().then(res=>{
        res.data.weekCount.forEach((item) => {
          this.barChartData.labels.push(item.date)
          this.barChartData.datasets[0].data.push(item.vulnerabilitesSum)
        })
        this.$refs.volFolib.buildData()
        this.weekCompare=res.data.compare
      })
      mounthDayCount().then(res=>{
        res.data.forEach((item) => {
          this.lineChartData.labels.push(item.date)
          this.lineChartData.datasets[0].data.push(item.denpendencySum)
          this.lineChartData.datasets[1].data.push(item.vulnerabilitesSum)
        })
        this.$refs.d30map.buildData()
      })
    },
    LayoutTypeBuild(record) {
      // console.log(getLayoutType(this.folibRepository))
      return getLayoutType2(record.layout,record.repository,'black')
    },
    goToDetial(item){
      storage.set("scannerView_repository",{item})
      this.$router.push({
        name: 'scannerDetial'
      })
    },
    getVulnerabilityPage (){
      vulnerabilityPage(this.vulnerabilityQuery).then(res=>{
        this.vulnerabilityData = res.data.rows
        this.vulnerabilityQuery.total = res.data.total
      })
    },
    handleVulnerabilityTableChange (pagination){
       this.vulnerabilityQuery.page = pagination.current
       this.getVulnerabilityPage()
    },
    downExcel (record){
      vulnerabilityExportExcel({vulnerabilityUuid:record.uuid}).then(res => {
        if(!res){
          return
        }
        const blob = new Blob([res], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' })
        let fileName = "漏洞" + record.uuid + "的影响范围.xlsx"
        if (window.navigator.msSaveOrOpenBlob) {
            //兼容IE10
            navigator.msSaveBlob(blob, fileName)
        } else {
          const url = window.URL.createObjectURL(blob)
          const link = document.createElement('a')
          link.href = url
          link.setAttribute('download', fileName)    
          document.body.appendChild(link)
          link.click()
          document.body.removeChild(link)
          window.URL.revokeObjectURL(url)
        }
      })
      .catch(err => {
        console.log(err)
      })
    },
    showGraph (record){

    },
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
}

.table-avatar-info .ant-avatar {
  margin-right: 8px;
}

.description{
  width: 95%;
  border: none;
  box-shadow: none;
  resize: none;
  background: #fbfbfb;
  vertical-align: middle;
}
.description-title{
  vertical-align: middle;
}
.v-btn{
  margin-right: 5px;
  width: 90px;
}
</style>