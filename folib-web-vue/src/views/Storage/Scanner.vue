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
                  :suffix="((countData.totalCount.onScanCount/(countData.totalCount.onScanCount+countData.totalCount.notScanCount))*100).toFixed(2)+'%'"
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
                  :suffix="((countData.totalCount.onScanAndScaned/(countData.totalCount.onScanAndScaned+countData.totalCount.onScanAndUnScan+countData.totalCount.onScanAndScanFailed))*100).toFixed(2)+'%'"
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
                  :suffix="((countData.denpendencyCount.vulnerableSum/(countData.totalCount.onScanCount))*100).toFixed(2)+'%'"
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
                  :suffix="((countData.denpendencyCount.vulnerabilitesSum/(countData.denpendencyCount.denpendencySum))*100).toFixed(2)+'%'"
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
                <a-row type="flex" align="middle">
                  <a-col :span="24" :md="12">
                    <h6>仓库扫描情况</h6>
                  </a-col>
                  <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                  </a-col>
                </a-row>
              </template>
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

import {getCount,getScannerSumDifVoList,weekDayCount,mounthDayCount} from "@/api/folib";
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
        denpendencyCount: {denpendencySum: 531, vulnerableSum: 34, vulnerabilitesSum: 46, suppressedSum: 0},
        totalCount: {onScanCount: 479, onScanAndUnScan: 0, onScanAndScanFailed: 0, notScanCount: 0,onScanAndScaned:0}
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
      folibScanData:[],
      weekCompare:{}
    }
  },
  methods: {
    getCountData() {
      getCount().then(res => {
        this.countData = res.data
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