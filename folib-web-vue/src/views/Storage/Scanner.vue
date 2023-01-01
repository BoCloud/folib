<!--
	This is the Dashboards default page, it uses the dashboard layout in:
	"./layouts/Dashboard.vue" .
 -->

<template>
  <div class="dashboard scanner">
    <a-row :gutter="24" type="flex" align="stretch">
      <a-col :span="24" :lg="24">
        <a-row :gutter="24">
          <a-col :span="24" :lg="24" :xl="6" class="mb-24" style="position: relative; z-index: 1;">
            <!-- Widget 1 Card -->
            <a-card :bordered="false" class="widget-1">
              <a-statistic
                  title="扫描包总数"
                  :value="countData.scanCount"
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
                  :value="countData.notScanCount"
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
                  :value="countData.scanSuccessCount"
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
                  :value="countData.scanFailCount"
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
                  :value="countData.dependencyCount"
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
                  :value="countData.dependencyVulnerabilitiesCount"
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
                  :value="countData.vulnerabilitiesCount"
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
                  :value="countData.suppressedVulnerabilitiesCount"
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
                <a-tabs class="tabs-sliding" default-active-key="1" @change="tabChange($event)">
                  <a-tab-pane key="1" tab="仓库扫描情况">
                    <a-table rowKey="id" :columns="columns" :data-source="folibScanData" :pagination="false">
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
                    <Vulnerability :vulnerabilityColumns="vulnerabilityColumns" :vulnerabilityLevel="1" :queryStorageId="false" :queryRepositoryId="false" ref="vulnerability"></Vulnerability>
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
            <p>较上周包数量 <span class="text-success">{{weekCompare.scanCount>0?'+'+weekCompare.scanCount:weekCompare.scanCount===0?'不变':'未知'}}</span></p>
          </div>
          <div class="card-content">
            <p>以下为本周与上周(14天)的数据进行比较的结果</p>
          </div>
          <a-row class="card-footer" type="flex" justify="center" align="top">
            <a-col :span="6">
              <h6>{{weekCompare.dependencyVulnerabilitiesCount}}</h6>
              <span>漏洞包新增</span>
            </a-col>
            <a-col :span="6">
              <h6>{{weekCompare.dependencyCount}}</h6>
              <span>扫描依赖新增</span>
            </a-col>
            <a-col :span="6">
              <h6>{{weekCompare.vulnerabilitiesCount}}</h6>
              <span>漏洞依赖新增</span>
            </a-col>
            <a-col :span="6">
              <h6>{{weekCompare.suppressedVulnerabilitiesCount}}</h6>
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

import {getCount,getScannerSumDifVoList,weekDayCount,mounthDayCount} from "@/api/folib";
import {getLayoutType2} from "@/utils/layoutUtil";
import ChartBar from '@/components/Charts/ChartBar';
import ChartLine from '@/components/Charts/ChartLine';
import Vulnerability from '@/components/Vulnerabilities/Vulnerability';
import storage from "store";
import store from '@/store';
import insertCss from 'insert-css';



export default ({
  components: {
    ChartBar,
    ChartLine,
    Vulnerability,
  },
  created() {
    this.userInfo=store.state.user
    this.getCountData()
  },
  data() {
    return {
      userInfo:{},
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
        scanCount: 0, 
        notScanCount: 0, 
        scanSuccessCount: 0, 
        unScanCount: 0,
        scanFailCount: 0, 
        dependencyCount: 0, 
        dependencyVulnerabilitiesCount: 0, 
        vulnerabilitiesCount: 0,
        suppressedVulnerabilitiesCount: 0,
      },
      activeKey: "1",
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
          dataIndex: 'scanCount',
          width: 100,
        },
        {
          title: '问题包数',
          dataIndex: 'dependencyVulnerabilitiesCount',
          width: 100,
        },
        {
          title: '漏洞数量',
          dataIndex: 'vulnerabilitiesCount',
          width: 100,
        },
        {
          title: '封存漏洞数量',
          dataIndex: 'suppressedVulnerabilitiesCount',
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
          title: '引入时间',
          dataIndex: 'created',
          scopedSlots: { customRender: 'created' },
          align: "center",
        },
        {
          title: 'CvssV2评分',
          dataIndex: 'cvssV2Score',
          scopedSlots: { customRender: 'cvssV2Score' },
          align: "center",
        },
        {
          title: 'CvssV2漏洞等级',
          dataIndex: 'cvssV2Severity',
          scopedSlots: { customRender: 'cvssV2Severity' },
          align: "center",
        },
        {
          title: 'CvssV3评分',
          dataIndex: 'cvssV3Score',
          scopedSlots: { customRender: 'cvssV3Score' },
          align: "center",
        },
        {
          title: 'CvssV3漏洞等级',
          dataIndex: 'cvssV3Severity',
          scopedSlots: { customRender: 'cvssV3Severity' },
          align: "center",
        },
        {
          title: '最高漏洞等级',
          dataIndex: 'highestSeverityText',
          scopedSlots: { customRender: 'highestSeverityText' },
          align: "center",
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
        this.countData = res
        if(this.countData.scanCount+this.countData.notScanCount > 0){
           this.onScanProportion = ((this.countData.scanCount/(this.countData.scanCount+this.countData.notScanCount))*100)
        }
        if(this.countData.scanSuccessCount+this.countData.unScanCount+this.countData.scanFailCount > 0){
           this.onScanAndScanedProportion = ((this.countData.scanSuccessCount/(this.countData.scanSuccessCount+this.countData.unScanCount+this.countData.scanFailCount))*100)
        }
        if(this.countData.scanCount > 0){
           this.vulnerableSumProportion = ((this.countData.dependencyVulnerabilitiesCount/(this.countData.scanCount))*100)
        }
        if(this.countData.dependencyCount > 0){
           this.vulnerabilitesSumProportion = ((this.countData.vulnerabilitiesCount/(this.countData.dependencyCount))*100)
        }
      })
      getScannerSumDifVoList().then(res =>{
        this.folibScanData=res
      })
      weekDayCount().then(res=>{
        res.dayCountList.forEach((item) => {
          this.barChartData.labels.push(item.date)
          this.barChartData.datasets[0].data.push(item.vulnerabilitiesCount)
        })
        if(this.$refs.volFolib){
          this.$refs.volFolib.buildData()
        }
        this.weekCompare=res.compareCount
      })
      mounthDayCount().then(res=>{
        res.forEach((item) => {
          this.lineChartData.labels.push(item.date)
          this.lineChartData.datasets[0].data.push(item.dependencyCount)
          this.lineChartData.datasets[1].data.push(item.vulnerabilitiesCount)
        })
        if(this.$refs.d30map){
          this.$refs.d30map.buildData()
        }
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
    tabChange(activeKey){
      if(activeKey === '2'){
        if(this.$refs.vulnerability){
          this.$refs.vulnerability.getVulnerabilityPage();
        }
      }
    },
    successMsg(message){
      if(!message){
        message = "操作成功"
      }
      this.$notification["success"]({
        message: message,
        description: ""
      })
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

}
</style>