<template>
  <div v-if="artifactData" id="pdf-content">
    <a-card :bordered="false" style="margin-bottom: 20px; position:relative;" :style="showBtn?'padding-right: 50px;':''">
      <a-row style="display: flex; justify-content: space-between">
        <a-col style="flex: 1; display: flex">
          <a-icon
            style="
              width: 56px;
              height: 56px;
              font-size: 30px;
              background-color: #20a8d8;
              color: #fff;
              line-height: 60px;
              float: left;
              margin-right: 10px;
            "
            type="apartment"
          />
          <div class="h5 title">
            {{ artifactName }}
          </div>
        </a-col>
        <a-col style="display: flex">
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityUnassigned"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'总数'"
            >{{ artifactData.artifact.vulnerabilitiesCount }}</vue-easy-pie-chart
          >
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityCritical"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'危急'"
            >{{ artifactData.artifact.criticalVulnerabilitiesCount }}</vue-easy-pie-chart
          >
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityHigh"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'高'"
            >{{ artifactData.artifact.highVulnerabilitiesCount }}</vue-easy-pie-chart
          >
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityMedium"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'中危'"
            >{{ artifactData.artifact.mediumVulnerabilitiesCount }}</vue-easy-pie-chart
          >
          <vue-easy-pie-chart
            style="margin-right: 1rem"
            :bar-color="severityLow"
            font-size="14px"
            :track-color="trackColor"
            scale-color=""
            line-cap="round"
            :line-width="3"
            :percent="100"
            :size="50"
            :animate="true"
            :title="'低'"
            >{{ artifactData.artifact.lowVulnerabilitiesCount }}</vue-easy-pie-chart
          >
        </a-col>
      </a-row>
      <div v-if="showBtn" class="export_excel_sty" :title="$t('Artifacts.exportPdf')" @click="exportPdf"> 
        <a-spin :spinning="confirmLoading">
          <img src="./export-pdf.svg" width="25" />
        </a-spin>
      </div>
    </a-card>

    <a-tabs class="tabs-sliding" default-active-key="1" @change="handleChangeTabs">
      <a-tab-pane key="1" :tab="$t('Artifacts.GeneralView')">
        <ArtifactDashboard :artifactData="artifactData" v-if="tabActive == 1"></ArtifactDashboard>
      </a-tab-pane>
      <a-tab-pane key="2" :tab="$t('Artifacts.Component')">
        <ArtifactComponents :artifactData="artifactData" v-if="tabActive == 2"></ArtifactComponents>
      </a-tab-pane>
      <a-tab-pane key="3" :tab="$t('Artifacts.Vulnerability')">
        <ArtifactVulnerability :artifactData="artifactData" v-if="tabActive == 3"></ArtifactVulnerability>
      </a-tab-pane>
    </a-tabs>
  </div>
</template>

<script>
import {
  getArtifact,
} from "@/api/folib";
import VueEasyPieChart from "vue-easy-pie-chart";
import html2canvas from 'html2canvas';
import jsPDF from 'jspdf';
import "vue-easy-pie-chart/dist/vue-easy-pie-chart.css";
import ArtifactDashboard from "./ArtifactDashboard.vue";
import ArtifactComponents from "./ArtifactComponents.vue";
import ArtifactVulnerability from "./ArtifactVulnerability.vue";
export default {
  components: { VueEasyPieChart, ArtifactDashboard, ArtifactComponents, ArtifactVulnerability },

  data() {
    return {
      tabActive: 1,
      artifactData: {artifact: {}},
      severityCritical: "#f86c6b",
      severityHigh: "#fd8c00",
      severityMedium: "#ffc107",
      severityLow: "#4dbd74",
      severityUnassigned: "#777777",
      severityInfo: "#20a8d8",
      trackColor: "#17232f",
      artifactName: "",
      showBtn:true,
      confirmLoading:false
    };
  },
  created() {
    this.initialize();
  },
  watch: {
    $route(to, from) {
      // console.log("路由变化了");
      this.initialize();
    },
    deep: true,
  },
  methods: {
    initialize() {
      let data = this.$route.query.data
      if (data) {
        this.queryArtifact(JSON.parse(data))
      }
    },
    exportPdf(){
      if(this.confirmLoading){
        return
      }
      const pdfContent = document.getElementById('pdf-content');
      this.confirmLoading = true
      // this.$notification.open({
      //   class: 'ant-notification-success',
      //   message: this.$t('Artifacts.exporting'),
      // });
      // 隐藏按钮
      // this.showBtn = false
      this.$nextTick(() => {
        // 使用 html2canvas 截取页面内容
        html2canvas(pdfContent, {
          scale: 2, // 提高图像质量
          useCORS: true, // 允许跨域资源
          scrollX: 0,
          scrollY: -window.scrollY, // 处理滚动位置
        }).then((canvas) => {
          // 获取内容的高度
          const contentHeight = canvas.height;
  
          // 创建 jsPDF 实例
          const pdf = new jsPDF('p', 'mm', 'a4'); // A4 尺寸（210mm x 297mm）
          const pdfWidth = 210; // A4 宽度（单位：mm）
          const pdfHeight = (contentHeight * pdfWidth) / canvas.width; // 动态计算 PDF 高度
  
          // 如果高度超过 A4 大小，按比例缩放
          if (pdfHeight > 297) {
            const scaleFactor = 297 / pdfHeight;
            pdfHeight = 297;
            canvas = this.scaleCanvas(canvas, scaleFactor);
          }
  
          const imgData = canvas.toDataURL('image/png');
          
          // 将图像添加到 PDF 中
          pdf.addImage(imgData, 'PNG', 0, 0, pdfWidth, pdfHeight); 
          pdf.save( `${this.$t('Artifacts.ProductScanDetail')}.pdf`); // 导出 PDF 文件
          // 显示按钮
          this.showBtn = true
          this.confirmLoading = false
        });
      })
    },

    // 缩放 canvas 以适配 PDF 页面
    scaleCanvas(canvas, scaleFactor) {
      const scaledCanvas = document.createElement('canvas');
      const ctx = scaledCanvas.getContext('2d');
      scaledCanvas.width = canvas.width * scaleFactor;
      scaledCanvas.height = canvas.height * scaleFactor;
      ctx.scale(scaleFactor, scaleFactor);
      ctx.drawImage(canvas, 0, 0);
      return scaledCanvas;
    },
    handleClickMenu(p) {
      this.$router.push(`/artifacts/artifactsDetail/${p.uuid}`);
      this.initialize();
    },
    handleChangeTabs(val) {
      this.tabActive = val;
    },
    queryArtifact(data) {
      getArtifact(
        data.layout,
        data.storageId,
        data.repositoryId,
        data.artifactPath
      ).then((res) => {
        this.artifactData = res
        this.artifactData.layout = data.layout
        if (data.layout.toLowerCase() !== "docker") {
          let arr = this.artifactData.artifact.artifactPath.split("/")
          this.artifactName = arr[arr.length -1]
        } else {
          this.artifactName = this.artifactData.artifact.artifactCoordinates.imageName
        }
      });
    }
  },
};
</script>

<style lang="scss" scoped>
.title {
  margin-top: 2px;
}
.dropdown {
  margin-left: 10px;
}

// ::v-deep .vue-easy-pie-chart .inner-text {
//   position: absolute;
//   top: 0;
//   left: 0;
//   right: 0;
//   bottom: 0;
//   text-align: center;
//   display: block;
// }

::v-deep .ant-card-head {
  padding: 0 !important;
}
::v-deep .ant-tabs-nav-wrap {
  padding: 0 24px;
}

.export_excel_sty{
  position: absolute;
  right: 20px;
  top: 15px;
  background: #fff;
  margin-top: 4px;
  width: 43px;
  height: 43px;
  padding: 9px;
  border-radius: 6px;
  box-shadow: 0px 1px 6px 2px rgba(0, 0, 0, 0.1);

  &:hover{
    box-shadow: 0px 1px 6px 2px rgba(0, 0, 0, 0.15);
    cursor: pointer;
  }
}
</style>
