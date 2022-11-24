<template>
  <div id="settings">
    <a-tabs class="tabs-sliding" default-active-key="1" @change="tabChange($event)">

      <a-tab-pane key="1" tab="基础监控">
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
                  <p>系统最大空间:{{ monitorData.disktotal }}</p>
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
                  <p>执行时间:{{ monitorData.gcpause.total.toFixed(2) }}s</p>
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
      </a-tab-pane>
      <a-tab-pane key="2" tab="系统日志">
        <a-card class="header-solid"
          :bodyStyle="{ padding: '50px', height: '1000px', display: 'flex', alignItems: 'center', justifyContent: 'center' }">
          <template #title>
            <a-row type="flex" align="middle">
              <a-col :span="24" :md="12">
                <h6 class="font-semibold m-0">在线日志查看</h6>
              </a-col>
              <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                <a-button type="primary" @click="viewLogs()">
                  同步
                </a-button>
              </a-col>
            </a-row>
          </template>
          <prism-editor class="my-editor" v-model="logs" :highlight="highlighterHandle" :line-numbers="false"
            :readonly="true"></prism-editor>
        </a-card>
      </a-tab-pane>
      <a-tab-pane key="3" tab="数据查询">
        <a-row type="flex" :gutter="[24, 24]">
          <a-col :span="24" :lg="6">
            <a-affix :offset-top="navbarFixed ? 100 : 10">
              <a-card :bordered="false" class="header-solid mb-24" id="result"
                :bodyStyle="{ height: '600px', display: 'flex', alignItems: 'center', justifyContent: 'center' }">
                <template #title>
                  <h5 class="mb-0 font-semibold">查询日志</h5>
                </template>
                <prism-editor class="my-editor2" v-model="gremlinResult" :highlight="highlighterHandle"
                  :line-numbers="true" :readonly="true"></prism-editor>
                <!--                <div class="text item" v-html="gremlinResult" id="result-data">-->
                <!--                </div>-->
              </a-card>
            </a-affix>
          </a-col>
          <a-col :span="24" :lg="18">
            <!-- Basic Info card -->
            <a-card :bordered="false" id="basic" class="header-solid mb-24">
              <template #title>
                <a-row type="flex" align="middle">
                  <a-col :span="24" :md="12">
                    <h6 class="font-semibold m-0">Gremlin查询</h6>
                  </a-col>
                  <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                    <a-button type="primary" @click="getGraphData()">
                      查询
                    </a-button>
                  </a-col>
                </a-row>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="24">
                    <a-form-item class="mb-10" label="Gremlin" :colon="false">
                      <prism-editor class="my-editor3" v-model="gremlin" :highlight="highlighterHandle"
                        :line-numbers="true"></prism-editor>
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>说明:</p>
                <ul class="pl-15 text-muted">
                  <li>Gremlin查询语言请看官方文档</li>
                  <li>默认查询内置的Gremlin数据库server端口为8182</li>
                </ul>
              </a-form>
            </a-card>
            <a-card :bordered="false" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">拓扑结果</h5>
              </template>
              <div class="g6-x" id="containerG6" ref="containerG6"></div>
            </a-card>

          </a-col>
        </a-row>
      </a-tab-pane>
      <a-tab-pane key="4" tab="集群信息" class="cluster">
        <a-row :gutter="24" type="flex" align="stretch">
          <a-col :span="24" class="text-left mb-10" v-if="repair">
            <a-button type="primary" @click="cassandraRepair()">
              数据修复
            </a-button>
          </a-col>
          <a-col :span="24" :xl="24" class="mb-24" v-for="(dc, dcKey, dcIndex) in cassandraClusterInfo.dcsMap"
            :key="dcIndex">
            <a-row :gutter="24" type="flex" align="stretch">
              <a-col :span="12" :xl="6" class="mb-24" v-for="(item, index) in dc" :key="index">
                <a-card :bordered="false"
                  :class="item.endpointWithPort.hostAddressAndPort == cassandraClusterInfo.endpoint ? 'header-solid h-full card-profile-information current-node' : 'header-solid h-full card-profile-information'"
                  :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0, }">
                  <template #title>
                    <a-card-meta title="节点信息" :description="'IP：' + item.endpoint">
                      <a-avatar slot="avatar" src="images/folib/cluster_node.svg" />
                    </a-card-meta>
                  </template>
                  <a-popconfirm slot="extra" title="危险操作！！！确定要移除此节点吗？" okType="danger" ok-text="确定" cancel-text="取消"
                    v-if="cassandraClusterInfo.unreachableNodeList.indexOf(item.endpointWithPort.hostAddressAndPort) != -1"
                    class="remove-node-popconfirm"
                    @confirm="removeNode(cassandraClusterInfo.hostIDMap[item.endpointWithPort.hostAddressAndPort], item.endpointWithPort.hostAddressAndPort)">
                    <a-icon type="poweroff" :style="{ fontSize: '24px', color: '#ff4d4f' }" />
                  </a-popconfirm>
                  <p class="text-dark">
                  </p>
                  <hr class="my-25">
                  <a-descriptions title="基本信息" :column="1">
                    <a-descriptions-item label="状态">
                      <a-badge
                        v-if="cassandraClusterInfo.liveNodeList.indexOf(item.endpointWithPort.hostAddressAndPort) != -1"
                        color="#87d068" text="在线" />
                      <a-badge
                        v-if="cassandraClusterInfo.unreachableNodeList.indexOf(item.endpointWithPort.hostAddressAndPort) != -1"
                        color="#f50" text="离线" />
                    </a-descriptions-item>
                    <a-descriptions-item label="使用端口">
                      {{ item.endpointWithPort.port }}
                    </a-descriptions-item>
                    <a-descriptions-item label="数据表总数">
                      {{ cassandraClusterInfo.statsHolderMap.total_number_of_tables }}
                    </a-descriptions-item>
                    <a-descriptions-item label="数据所有权">
                      <span v-if="item.owns">{{ (item.owns * 100).toFixed(2) + '%' }}</span>
                    </a-descriptions-item>
                    <a-descriptions-item label="预估数据量">
                      {{ cassandraClusterInfo.loadMap[item.endpointWithPort.hostAddressAndPort] }}
                    </a-descriptions-item>
                  </a-descriptions>
                </a-card>
              </a-col>
            </a-row>
          </a-col>
        </a-row>
      </a-tab-pane>
    </a-tabs>
  </div>
</template>
<script>
import {
  fileSizeConver
} from '@/utils/layoutUtil'
import { getMetrics, getMetricsHealth, viewLogs, gremlinQuery, gremlinVertex, gremlinEdge, getCassandraClusterInfo, cassandraRemoveNode, cassandraRepair } from "@/api/monitor";
// Importing charts
import ChartLineGradient from '@/components/Charts/ChartLineGradient';
import { PrismEditor } from 'vue-prism-editor'
import 'vue-prism-editor/dist/prismeditor.min.css' // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from 'prismjs/components/prism-core'
import 'prismjs/components/prism-clike'
import 'prismjs/components/prism-javascript'
import 'prismjs/themes/prism-tomorrow.css'
// import vis from 'vis-network'
import G6 from '@antv/g6'
export default {
  props: ['navbarFixed'],
  data() {
    return {
      logs: "",
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
      queyLogs: "",
      gremlin: "g.E().limit(100)",
      gremlinResult: null,
      graphData: {
        // 点集
        nodes: [
        ],
        // 边集
        edges: [

        ]
      },
      graph: null,
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
      repair: false,
      cassandraClusterInfo: {
        endpoint: "",
        localHostId: "",
        hostIDMap: {},
        loadMap: {},
        statsHolderMap: {},
        tokenList: [],
        unreachableNodeList: [],
        joiningNodeList: [],
        liveNodeList: [],
        movingNodeList: [],
        leavingNodeList: [],
        ownershipMap: {},
        effectiveOwnershipMap: {},
        seedList: [],
        gossipInfo: "",
        dcsMap: {
          "FOLIB-DC1": [
            {
              endpoint: "",
              resolveIp: true,
              owns: undefined,
              token: "",
              endpointWithPort: {
                address: "",
                addressBytes: "",
                port: undefined,
                hostAddressAndPort: "",
                hostAddressAndPortForJMX: ""
              }
            }
          ]
        },
        tokensToEndpointsMap: {},
      },
    };
  },
  components: {
    ChartLineGradient,
    PrismEditor,
  },
  created() {

  },
  mounted() {
    this.getMetrics()
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
    viewLogs() {
      viewLogs().then(res => {
        this.logs = res
      })
    },

    gremlinQuery() {
      let wh = document.documentElement.clientHeight;
      // let eh = 247;
      // let ch = (wh - eh) + "px";
      // document.getElementById("result").style.minHeight = ch;
      let c = 500;
      let gh = (wh - c) + "px";
      document.getElementById("graph").style.height = gh;
      gremlinQuery(this.gremlin, "g").then(res => {
        var result = res;
        this.gremlinResult = result.result;
        var container = document.getElementById('graph');
        var nodes = new vis.DataSet(result.vertices);
        var edges = new vis.DataSet(result.edges);
        var data = {
          nodes: nodes,
          edges: edges
        };
        var options = {
          edges: {
            arrows: {
              to: {
                type: "arrow",
                enabled: true
              }
            }
          },
          nodes: {
            shape: 'circle'
          },
          interaction: {
            selectConnectedEdges: false
          }
        };
        var network = new vis.Network(container, data, options);
        var notification = this.$notification;
        network.on("selectNode", function (params) {
          var nid = params.nodes[0];
          // Notification.closeAll();
          // this.$notification.closeAllNodes()
          var node = nodes._data.get(nid);
          var title = "id:" + node.id + ",\tlabel:" + node.label;
          gremlinVertex(nid, "g").then(res => {
            var result = res;
            var c = "";
            var values = result.keyValues;
            for (var i = 0; i < values.length; i++) {
              c += values[i].key + ":\n " + values[i].value + "\n";
            }

            notification.open({
              message: title,
              description: c,
              placement: 'bottomRight',
              duration: 3
            });
          }).catch(function () { });
        });
        network.on("selectEdge", function (params) {
          var eid = params.edges[0];
          // this.$notification.close()
          var edge = edges._data.get(eid);
          var title = "id:" + edge.id + ",\tlabel:" + edge.label;
          gremlinEdge(eid, "g").then(res => {
            var result = res;
            var c = "";
            var values = result.keyValues;
            for (var i = 0; i < values.length; i++) {
              c += values[i].key + ":&emsp;" + values[i].value + "<br/>";
            }
            notification.open({
              message: title,
              description: c,
              placement: 'bottomRight',
            });
            // Notification({
            //   title: title,
            //   message: c,
            //   dangerouslyUseHTMLString: true,
            //   duration: 60000,
            //   customClass: 'prop-box',
            //   position: 'bottom-right'
            // });
          }).catch(function () { });
        });

      })
    },

    getGraphData() {
      //点击查询 通过接口获取数据
      gremlinQuery(this.gremlin, "g").then(res => {
        this.gremlinResult = res.result;
        this.graphData.nodes = res.vertices
        this.graphData.edges = res.edges
        console.log(this.graphData)
      }).then(() => {
        if (this.graph) {
          //如果存在画布，销毁画布，重新渲染
          this.graph.destroy()
        }
        this.initGraph()
      })
    },

    initGraph() {
      // 初始化图
      // 假数据
      const G6data = {
        nodes: [
          {
            id: '0',
            label: '0',
          },
          {
            id: '1',
            label: '1',
          },
          {
            id: '2',
            label: '2',
          },
        ],
        edges: [
          {
            source: '0',
            target: '1',
          },
          {
            source: '1',
            target: '2',
          },
          {
            source: '1',
            target: '0',
          },
        ],
      };
      // 点击节点展示提示框
      const tooltip = new G6.Tooltip({
        offsetX: 10,
        offsetY: 10,
        trigger: 'click',
        // 允许出现 tooltip 的 item 类型
        itemTypes: ['node', 'edge'],
        // 自定义 tooltip 内容
        getContent: (e) => {
          const outDiv = document.createElement('div');
          outDiv.style.width = 'fit-content';
          //outDiv.style.padding = '0px 0px 20px 0px';
          outDiv.innerHTML = `
      <ul>
        <li>Type: ${e.item.getType()}</li>
      </ul>
      <ul>
        <li>Label: ${e.item.getModel().label || e.item.getModel().id}</li>
      </ul>`;
          return outDiv;
        },
      });
      // 【步骤4】 创建关系图
      const containerG6 = this.$refs.containerG6 // 获取容器（DOM元素）
      const width = containerG6.offsetWidth // Number，必须，图的宽度
      const height = containerG6.offsetHeight // Number，必须，图的宽度
      this.graph = new G6.Graph({
        container: 'containerG6', // String | HTMLElement，必须，在 Step 1 中创建的容器 id 或容器本身
        height,
        width,
        plugins: [tooltip], // 提示框
        modes: {
          default: ['drag-canvas'] // 拖拽画布
        },
        defaultEdge: {
          type: 'quadratic', // 指定边的形状为二阶贝塞尔曲线
          style: {
            // 箭头
            endArrow: true,
            startArrow: true
          },
        },
      })
      // 鼠标事件
      this.graph.on('node:mouseenter', (e) => {
        this.graph.setItemState(e.item, 'active', true);
      });
      this.graph.on('node:mouseleave', (e) => {
        this.graph.setItemState(e.item, 'active', false);
      });
      this.graph.on('edge:mouseenter', (e) => {
        this.graph.setItemState(e.item, 'active', true);
      });
      this.graph.on('edge:mouseleave', (e) => {
        this.graph.setItemState(e.item, 'active', false);
      });
      // 【步骤5】 匹配数据源并渲染
      this.graph.read(this.graphData) // 读取 Step 2 中的数据源到图上
    },
    successMsg(message) {
      if (!message) {
        message = "操作成功"
      }
      this.$notification["success"]({
        message: message,
        description: ""
      })
    },
    queryCassandraClusterInfo() {
      getCassandraClusterInfo().then(res => {
        this.cassandraClusterInfo = res
        if (this.cassandraClusterInfo.hostIDMap) {
          let hostIDKeys = Object.keys(this.cassandraClusterInfo.hostIDMap)
          if (hostIDKeys.length > 1) {
            this.repair = true
          } else {
            this.repair = false
          }
        }
      })
    },
    tabChange(active) {
      if (active == 4) {
        this.queryCassandraClusterInfo()
      }
    },
    removeNode(hostId, endpoint) {
      const key = 'removeNode_' + hostId
      const flag = Storage.prototype.getCanExpireLocal(key);
      if (flag) {
        console.log("=====>>>>>存在", key)
        return
      }
      Storage.prototype.setCanExpireLocal(key, true, 30 * 1000)
      this.successMsg("正在移除节点 " + endpoint + "，请稍候刷新页面查看")
      cassandraRemoveNode(hostId).then(res => {
        this.successMsg("成功移除节点：" + endpoint)
      }).catch((err) => {

      })
    },
    cassandraRepair() {
      cassandraRepair().then(res => {
        this.successMsg("集群数据修复完成");
      }).catch((err) => {
        this.$notification["error"]({
          message: err.response.data.error,
        })
      })
    }
  },

  beforeDestroy() {
    //如果定时器还在运行 或者直接关闭，不用判断
    clearInterval(this.timer); //关闭
  },
};
</script>

<style lang="scss" scoped>
#settings::v-deep {
  .my-editor {
    background: #171717;
    color: #b7b6b6;

    font-family: Fira code, Fira Mono, Consolas, Menlo, Courier, monospace;
    font-size: 12px;
    line-height: 1.5;
    padding: 5px;
  }

  .my-editor3 {
    background: #030303;
    color: #e8e8e8;

    font-family: Fira code, Fira Mono, Consolas, Menlo, Courier, monospace;
    font-size: 12px;
    line-height: 1.5;
    padding: 5px;
  }

  .my-editor2 {
    background: #fafafa;
    color: #595959;

    font-family: Fira code, Fira Mono, Consolas, Menlo, Courier, monospace;
    font-size: 12px;
    line-height: 1.5;
    padding: 5px;
  }

  .g6-x {
    width: 800px;
    height: 500px;
    box-sizing: border-box;
    margin-left: 20px;
  }

  .ant-list {
    width: 100%;
  }

  .ant-list-item-meta-avatar {
    margin-right: 8px;
  }

  .ant-list-item-meta {
    align-items: center;
  }

  .ant-list-item-meta-title {
    margin: 0;
  }

  .ant-anchor-ink::before {
    display: none;
  }

  .ant-anchor-link {
    padding: 0;
    margin-top: 8px;
  }

  .ant-anchor-link a {
    width: 100%;
    border-radius: 8px;
    color: #67748e !important;
    padding: 10px 16px;
    background-color: transparent;
    transition: background-color 0.3s ease-in;
  }

  .ant-anchor-link a:hover {
    background-color: #eeeeee;
  }

  .ant-anchor-link a svg g {
    fill: #344767;
  }

  .ant-anchor-link a svg {
    margin-right: 8px;
  }

  .card-profile-head {
    margin: 0 0 24px;
  }

  .tags-field .ant-form-item-control {
    line-height: 33px;
  }

  .form-tag.ant-tag {
    border-radius: 20px;
    padding: 4px 10px;
    font-size: 12px;
    font-weight: 500;
    margin-right: 3.75px;
    margin-bottom: 3.75px;
    background-color: #3a416f;
    border: 1px solid #3a416f;
    color: #fff;
  }

  .form-tag.ant-tag .anticon-close {
    color: #fff;
    height: 16px;
    border-left: 1px solid hsla(0, 0%, 100%, .3);
    padding-left: 5px;
    padding-top: 2px;
    opacity: .75;
  }

  .form-tag.ant-tag .anticon-close:hover {
    color: #fff;
    opacity: 1;
  }

  .tags-field .ant-input {
    margin-bottom: 5px;
    margin-top: 4px;
  }

  .tags-field .ant-select {
    .ant-select-selection__choice__remove i {
      color: #fff;
      height: 16px;
      border-left: 1px solid hsla(0, 0%, 100%, .3);
      padding-left: 5px;
      padding-top: 2px;
      opacity: .75;

      &:hover {
        color: #fff;
        opacity: 1;
      }
    }

    .ant-select-selection__rendered>ul>li:not(.ant-select-search) {
      border-radius: 20px;
      padding: 2px 27px 2px 10px;
      font-size: 12px;
      font-weight: 500;
      margin-right: 3.75px;
      margin-bottom: 3.75px;
      background-color: #3a416f;
      border: 1px solid #3a416f;
      color: #fff;
      line-height: 2;
      height: 30px;
    }

    .ant-select-selection--multiple {
      padding: 8px 10px;
    }
  }

  .cluster .ant-avatar img {
    height: unset;
  }

  .cluster .current-node {
    border: 3px solid #ddd;
    border-image: linear-gradient(#A18DFF, #005CFD) 1;
    clip-path: inset(0 round 6px);
  }

  .cluster .remove-node-popconfirm {
    margin: 0 16px 0 0;
    cursor: pointer;
  }
}
</style>