<template>
  <div >
        <a-row :gutter="24" type="flex" align="stretch">
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="monitorData.folibFilenOpen">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/process-files-open.svg" alt="">
                  </div>
                  <h6>句柄情况</h6>
                  <p>系统最大句柄为:{{ monitorData.fileOpenMax }}</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="monitorData.diskfree">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/disk.svg" alt="">
                  </div>
                  <h6>存储大小</h6>
                  <p>系统最大空间:{{ monitorData.disktotal |filterTwoNum}}</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="fileSizeConver(monitorData.jvmCommitted)">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/jvmCommitted.svg" alt="">
                  </div>
                  <h6>可用内存</h6>
                  <p>指JVM可用内存大小</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="monitorData.jettyCurrent">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/jetty.svg" alt="">
                  </div>
                  <h6>线程数量</h6>
                  <p>当前程序的线程数量</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="monitorData.loadAverage">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/loadAverage.svg" alt="">
                  </div>
                  <h6>系统负载</h6>
                  <p>当前系统的负载评估值</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
          <a-col :span="12" :xl="4" class="mb-24">
            <a-card :bordered="false" class="widget-2 h-full">
              <a-statistic :value="monitorData.gcpause.count + 's'">
                <template #title>
                  <div class="icon">
                    <img src="images/folib/gc.svg" alt="">
                  </div>
                  <h6>GC耗时</h6>
                  <p>执行时间:{{ monitorData.gcpause.total|filterTwoNum }}s</p>
                </template>
              </a-statistic>
            </a-card>
          </a-col>
        </a-row>
        <a-row :gutter="24" type="flex" align="stretch">
          <a-col :span="24" :lg="8" class="mb-24">
            <a-card :bordered="false" class="header-solid" :bodyStyle="{ padding: '0 12px 8px 3px' }">
              <template #title>
                <h6>CPU使用情况</h6>
              </template>
              <ChartLineGradient ref="cpu" :labels="cpuLabels" :dataOne="cpuDataOne" :dataTwo="cpuDataTwo"
                dataOneTag="系统CPU" dataTwoTag="Folib进程CPU" />
            </a-card>
          </a-col>
          <a-col :span="24" :lg="8" class="mb-24">
            <a-card :bordered="false" class="header-solid" :bodyStyle="{ padding: '0 12px 8px 3px' }">
              <template #title>
                <h6>内存使用情况(GB)</h6>
              </template>
              <ChartLineGradient ref="mem" :labels="memLabels" :dataOne="memDataOne" :dataTwo="memDataTwo"
                dataOneTag="最大可用内存" dataTwoTag="Folib当前使用内存" />
            </a-card>
          </a-col>
          <a-col :span="24" :lg="8" class="mb-24">
            <a-card :bordered="false" class="header-solid" :bodyStyle="{ padding: '0 12px 8px 3px' }">
              <template #title>
                <h6>JVM线程情况</h6>
              </template>
              <ChartLineGradient ref="thread" :labels="threadLabels" :dataOne="threadDataOne" :dataTwo="threadDataTwo"
                dataOneTag="活动线程数" dataTwoTag="BLOCKED" />
            </a-card>
          </a-col>

        </a-row>
        <!--        <a-card class="header-solid"-->
        <!--                :bodyStyle="{padding: '50px', height: '300px', display: 'flex', alignItems: 'center', justifyContent: 'center'}">-->
        <!--          <a href="#" class="text-center text-muted font-bold">-->
        <!--            <h6 class="font-semibold text-muted">安全策略用来配置Folib-Scanner相关的</h6>-->
        <!--          </a>-->
        <!--        </a-card>-->
  </div>
</template>
<script>
import {
  fileSizeConver
} from '@/utils/layoutUtil'
import { getMetrics, getMetricsHealth, viewLogs, getCassandraClusterInfo, cassandraRemoveNode, cassandraRepair } from "@/api/monitor";
// Importing charts
import ChartLineGradient from '@/components/Charts/ChartLineGradient';

export default {
    name:'BaseData',
  data() {
    return {
      cpuLabels: [],
      cpuDataOne: [],
      cpuDataTwo: [],
      memLabels: [],
      memDataOne: [],
      memDataTwo: [],
      threadLabels: [],
      threadDataOne: [],
      threadDataTwo: [],
      timer: null,
      monitorData: {
        fileOpenMax: 0,
        folibFilenOpen: 0,

        disktotal: 0,
        diskfree: 0,

        jvmCommitted: 0,

        jettyCurrent: "",

        loadAverage: 0,

        gcpause: {
          count: 0,
          total: 0
        }
      },    
    };
  },
  components: {
    ChartLineGradient,
  },
  created() {
  },
  mounted() {
    this.getMetrics()
  },
  filters:{
    filterTwoNum(val){
      let realVal=''
      if(!inNaN(val)&&val!==''){
        realVal=parFloat(val).toFixed(2)
      }else{
        realVal='--'
      }
      return realVal
    }
  },
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js) //returns html
    },
    getMetrics() {
      this.timer = setInterval(() => {
        getMetrics("process.cpu.usage").then(res => {
          this.cpuDataTwo.push(res.measurements[0].value)
        })
        getMetrics("system.cpu.usage").then(res => {
          this.cpuDataOne.push(res.measurements[0].value)
          this.cpuLabels.push(this.cpuDataOne.length)
        })

        getMetrics("jvm.memory.max").then(res => {
          this.memDataOne.push(res.measurements[0].value / (1024 * 1024 * 1024))
        })
        getMetrics("jvm.memory.used").then(res => {
          this.memDataTwo.push(res.measurements[0].value / (1024 * 1024 * 1024))
          this.memLabels.push(this.memDataTwo.length)
        })

        getMetrics("jvm.threads.live").then(res => {
          this.threadDataOne.push(res.measurements[0].value)
        })
        getMetrics("jetty.threads.current").then(res => {
          this.threadDataTwo.push(res.measurements[0].value)
          this.threadLabels.push(this.threadDataTwo.length)
          this.monitorData.jettyCurrent = res.measurements[0].value
        })
        //jvm.threads.states   jvm.threads.live

        this.$refs.cpu.buildData()
        this.$refs.mem.buildData()
        this.$refs.thread.buildData()

        getMetrics("process.files.open").then(res => {
          this.monitorData.folibFilenOpen = res.measurements[0].value
        })

        getMetrics("process.files.max").then(res => {
          this.monitorData.fileOpenMax = res.measurements[0].value
        })

        getMetricsHealth().then(res => {
          this.monitorData.disktotal = this.fileSizeConver(res.components.diskSpace.details.total)
          this.monitorData.diskfree = this.fileSizeConver(res.components.diskSpace.details.total - res.components.diskSpace.details.free)
        })

        getMetrics("jvm.memory.committed").then(res => {
          this.monitorData.jvmCommitted = res.measurements[0].value
        })

        getMetrics("system.load.average.1m").then(res => {
          this.monitorData.loadAverage = res.measurements[0].value

        })

        getMetrics("jvm.gc.pause").then(res => {
          this.monitorData.gcpause.count = res.measurements[0].value
          this.monitorData.gcpause.total = res.measurements[1].value
        })

        //
      }, 3000)
      // getMetrics("system.load.average.1m").then(res=>{console.log(res)})
    },
    fileSizeConver(m) {
      return fileSizeConver(m)
    },
  },

  beforeDestroy() {
    //如果定时器还在运行 或者直接关闭，不用判断
    clearInterval(this.timer); //关闭
  },
};
</script>

<style lang="scss" scoped>

</style>