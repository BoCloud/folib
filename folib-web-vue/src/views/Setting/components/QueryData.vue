<template>
  <div>
    <a-row type="flex" :gutter="[24, 24]">
      <a-col :span="24" :lg="6">
        <a-affix :offset-top="navbarFixed ? 100 : 10">
          <a-card
            :bordered="false"
            class="header-solid mb-24"
            id="result"
            :bodyStyle="{
              height: '530px',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }"
          >
            <template #title>
              <h5 class="mb-0 font-semibold">查询日志</h5>
            </template>
            <prism-editor
              class="my-editor2"
              v-model="gremlinResult"
              :highlight="highlighterHandle"
              :line-numbers="true"
              :readonly="true"
            ></prism-editor>
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
              <a-col
                :span="24"
                :md="12"
                style="
                  display: flex;
                  align-items: center;
                  justify-content: flex-end;
                "
              >
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
                  <prism-editor
                    class="my-editor3"
                    v-model="gremlin"
                    :highlight="highlighterHandle"
                    :line-numbers="true"
                  ></prism-editor>
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
  </div>
</template>
<script>
const Colors = {
  MavenArtifactCoordinates: { back: "#f2e394", text: "#588c73" },
  Artifact: { back: "#d96459", text: "#fff" },
  ArtifactIdGroup: { back: "#A7C2FF", text: "#fff" },
  NpmArtifactCoordinates: { back: "#d96459", text: "#fff" },
  GenericArtifactCoordinates: { back: "#588c73", text: "#fff" },
  ArtifactTag: { back: "#D5FF86", text: "#fff" },
};
import {
  gremlinQuery,
  gremlinVertex,
  gremlinEdge,
  cassandraRemoveNode,
} from "@/api/monitor";
// Importing charts
import { PrismEditor } from "vue-prism-editor";
import "vue-prism-editor/dist/prismeditor.min.css"; // import the styles somewhere
// import highlighting library (you can use any library you want just return html string)
import { highlight, languages } from "prismjs/components/prism-core";
import "prismjs/components/prism-clike";
import "prismjs/components/prism-javascript";
import "prismjs/themes/prism-tomorrow.css";
// import vis from 'vis-network'
import G6 from "@antv/g6";
import { GraphLayoutPredict } from "@antv/vis-predict-engine";

export default {
  props: ["navbarFixed"],
  data() {
    return {
      logs: "",
      gremlin: "g.E().limit(100)",
      gremlinResult: null,
      graphData: {
        // 点集
        nodes: [],
        // 边集
        edges: [],
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
          total: 0,
        },
      },
    };
  },
  components: {
    PrismEditor,
  },
  mounted() {},
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js); //returns html
    },
    gremlinQuery() {
      let wh = document.documentElement.clientHeight;
      // let eh = 247;
      // let ch = (wh - eh) + "px";
      // document.getElementById("result").style.minHeight = ch;
      let c = 500;
      let gh = wh - c + "px";
      document.getElementById("graph").style.height = gh;
      gremlinQuery(this.gremlin, "g").then((res) => {
        var result = res;
        this.gremlinResult = result.result;
        var container = document.getElementById("graph");
        var nodes = new vis.DataSet(result.vertices);
        var edges = new vis.DataSet(result.edges);
        var data = {
          nodes: nodes,
          edges: edges,
        };
        var options = {
          edges: {
            arrows: {
              to: {
                type: "arrow",
                enabled: true,
              },
            },
          },
          nodes: {
            shape: "circle",
          },
          interaction: {
            selectConnectedEdges: false,
          },
        };
        var network = new vis.Network(container, data, options);
        var notification = this.$notification;
        network.on("selectNode", function (params) {
          var nid = params.nodes[0];
          // Notification.closeAll();
          // this.$notification.closeAllNodes()
          var node = nodes._data.get(nid);
          var title = "id:" + node.id + ",\tlabel:" + node.label;
          gremlinVertex(nid, "g")
            .then((res) => {
              var result = res;
              var c = "";
              var values = result.keyValues;
              for (var i = 0; i < values.length; i++) {
                c += values[i].key + ":\n " + values[i].value + "\n";
              }

              notification.open({
                message: title,
                description: c,
                placement: "bottomRight",
                duration: 3,
              });
            })
            .catch(function () {});
        });
        network.on("selectEdge", function (params) {
          var eid = params.edges[0];
          // this.$notification.close()
          var edge = edges._data.get(eid);
          var title = "id:" + edge.id + ",\tlabel:" + edge.label;
          gremlinEdge(eid, "g")
            .then((res) => {
              var result = res;
              var c = "";
              var values = result.keyValues;
              for (var i = 0; i < values.length; i++) {
                c += values[i].key + ":&emsp;" + values[i].value + "<br/>";
              }
              notification.open({
                message: title,
                description: c,
                placement: "bottomRight",
              });
              // Notification({
              //   title: title,
              //   message: c,
              //   dangerouslyUseHTMLString: true,
              //   duration: 60000,
              //   customClass: 'prop-box',
              //   position: 'bottom-right'
              // });
            })
            .catch(function () {});
        });
      });
    },

    getGraphData() {
      //点击查询 通过接口获取数据
      gremlinQuery(this.gremlin, "g")
        .then((res) => {
          this.gremlinResult = res.result;
          const globalFontSize = 8;

          this.graphData.nodes = res.vertices.map((p) => {
            p.data=JSON.parse(JSON.stringify(p));
            p.color = Colors[p.label].back;
            p.style = {
              fill: Colors[p.label].back,
              lineWidth: 0,
            };
            p.labelCfg = {
              style: {
                fontSize: 8,
                fill: Colors[p.label].text,
                fontWeight: 300,
              },
              position: "center",
            };
            p.label = this.fittingString(p.label, 45, globalFontSize);
            return p;
          });
          this.graphData.edges = res.edges.map((p) => {
            return { source: p.source.id, target: p.target.id };
          });
          console.log(this.graphData);
        })
        .then(() => {
          if (this.graph) {
            //如果存在画布，销毁画布，重新渲染
            this.graph.destroy();
          }
          this.initGraph();
        });
    },

    async initGraph() {
      // 初始化图
      // 假数据
      // 点击节点展示提示框
      const tooltip = new G6.Tooltip({
        offsetX: 10,
        offsetY: 10,
        trigger: "click",
        // 允许出现 tooltip 的 item 类型
        itemTypes: ["node", "edge"],
        // 自定义 tooltip 内容
        getContent: (e) => {
          const outDiv = document.createElement("div");
          outDiv.style.width = "fit-content";
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
      const containerG6 = this.$refs.containerG6; // 获取容器（DOM元素）
      const width = containerG6.offsetWidth; // Number，必须，图的宽度
      const height = containerG6.offsetHeight; // Number，必须，图的宽度

      //智能布局
      // const { predictLayout, confidence } = await GraphLayoutPredict.predict(
      //   this.graphData
      // );

      this.graph = new G6.Graph({
        container: "containerG6", // String | HTMLElement，必须，在 Step 1 中创建的容器 id 或容器本身
        height,
        width,
        layout: {
          // type: predictLayout,
          type: "force",
          linkDistance: 60,
          nodeSize: 45,
          preventOverlap: true,
        },
        plugins: [tooltip], // 提示框
        modes: {
          default: [
            {
              type: "zoom-canvas",
              enableOptimize: true,
              optimizeZoom: 0.9,
            },
            {
              type: "drag-canvas",
              enableOptimize: true,
            },
          ], // 拖拽画布
        },

        defaultNode: {
          size: 45,
          // labelCfg: {
          //   style: {
          //     fontSize: 8,
          //     // fill: "#656464",
          //   },
          //   position: "center",
          // },
        },
        nodeStateStyles: {
          active: {
            // keyShape 的状态样式
            fill: "#fff",
          },
        },
        defaultEdge: {
          type: "quadratic", // 指定边的形状为二阶贝塞尔曲线
          style: {
            // 箭头
            endArrow: true,
            startArrow: true,
          },
        },
      });
      // 鼠标事件
      this.graph.on("node:mouseenter", (e) => {
        // debugger;
        this.graph.setItemState(e.item, "active", true);
        this.graph.updateItem(e.item, {
          labelCfg: {
            style: {
              fill: "#000",
            },
          },
        });
      });
      this.graph.on("node:mouseleave", (e) => {
        this.graph.setItemState(e.item, "active", false);
       
        const color=Colors[e.item._cfg.model.data.label]  
        this.graph.updateItem(e.item, {
          labelCfg: {
            style: {
              fill: color.text,
            },
          },
        });
      });

      // 【步骤5】 匹配数据源并渲染
      // this.graph.read(this.graphData) // 读取 Step 2 中的数据源到图上
      // 读取数据
      this.graph.data(this.graphData);
      // 渲染图
      this.graph.render();
      // this.graph.data(this.graphData)
    },
    fittingString(str, maxWidth, fontSize) {
      let currentWidth = 0;
      let res = str;
      const pattern = new RegExp("[\u4E00-\u9FA5]+"); // distinguish the Chinese charactors and letters
      str.split("").forEach((letter, i) => {
        if (currentWidth > maxWidth) return;
        if (pattern.test(letter)) {
          // Chinese charactors
          currentWidth += fontSize;
        } else {
          // get the width of single letter according to the fontSize
          currentWidth += G6.Util.getLetterWidth(letter, fontSize);
        }
        if (currentWidth > maxWidth) {
          res = `${str.substr(0, i)}\n${str.substr(i)}`;
        }
      });
      return res;
    },
  },
};
</script>

<style lang="scss" scoped></style>
