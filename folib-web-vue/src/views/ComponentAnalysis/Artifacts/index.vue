<template>
  <div class="wrapper">
    <!-- <HeaderEcharts></HeaderEcharts> -->
    <a-card :bordered="false" style="margin-top: 10px; margin-bottom: 20px; overflow-y: auto" class="header-solid">
      <div class="mx-25 search">
        <a-row type="flex" justify="end">
          <a-col :span="3" class="ml-16">
            <a-cascader :placeholder="$t('Artifacts.RepositorySelect')" v-model="selectRepository"
              :showSearch="{ repositoryFilter }" :allowClear="true" :options="repositoryList"
              @change="repositoryChange" />
          </a-col>
          <a-col :span="3">
            <a-input-search :placeholder="$t('Artifacts.ArtifactPathQuery')" class="v-search"
              v-model="queryParams.artifactName" @search="handheTableSearch()" />
          </a-col>
          <a-col :span="1">
            <a-popconfirm :disabled="confirmLoading" :ok-text="$t('Artifacts.OK')" :cancel-text="$t('Artifacts.CANCEL')" @confirm="getExportData">
              <template #title>
                  <span style="font-weight: 600;font-size:14px;">{{ $t('Artifacts.printExportCount') }}</span>
                  <div style="padding-top: 10px;padding-bottom: 10px;">
                    <a-input
                      v-model:value="exportCount"
                      :placeholder="$t('Artifacts.printExportCount')"
                      style="width: 120px"
                      :max="1000"
                      :min="0"
                      @input="handleInput"
                      size="small"
                    />
                  </div>
              </template>
              <div class="export_excel_sty" :title="$t('Artifacts.exportExcel')"> 
                <a-spin :spinning="confirmLoading">
                  <img src="./export-excel.svg" width="20" />
                </a-spin>
              </div>
            </a-popconfirm>
          </a-col>
        </a-row>
      </div>
      <a-table rowKey="uuid" class="mt-20" :columns="i18nColumns" :data-source="artifactsData"
        @change="handleChangeTable" :scroll="{ x: true }" :loading="artifactsTableLoading"
        :pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }">
        <template slot="artifactPath" slot-scope="artifactPath, row">
          <a-button type="link" @click="handleGoDetail(row)">
            {{ artifactPath }}
          </a-button>
        </template>
        <!-- <template slot="classifier" slot-scope="classifier">
          <a-button type="link">
            {{ classifier }}
          </a-button>
        </template>
        <template slot="lastBomImport" slot-scope="lastBomImport">
          {{ typeof lastBomImport === "number" ? formatTimestamp(lastBomImport, true) : "-" }}
        </template> -->
        <template slot="sizeInBytes" slot-scope="sizeInBytes">
          {{ fileSizeConver(sizeInBytes) }}
        </template>
        <template slot="vulnerabilitiesCount" slot-scope="vulnerabilitiesCount, row">
          <a-tag color="#f86c6b">{{ row.criticalVulnerabilitiesCount }} </a-tag>
          <a-tag color="#fd8c00">{{ row.highVulnerabilitiesCount }} </a-tag>
          <a-tag color="#ffc107">{{ row.mediumVulnerabilitiesCount }} </a-tag>
          <a-tag color="#4dbd74"> {{ row.lowVulnerabilitiesCount }}</a-tag>
        </template>
        <!-- <template slot="lastBomImportFormat" slot-scope="lastBomImportFormat">
          {{ !lastBomImportFormat ? "-" : lastBomImportFormat }}
        </template> -->
      </a-table>
    </a-card>
  </div>
</template>

<script>
import HeaderEcharts from "../Components/HeaderEcharts";
import { getProjectsList } from "@/api/projects.js";
import { formatTimestamp } from "@/utils/util.js";
import {
  fql,
  queryOnScanTree
} from "@/api/folib";
import {
  getFileImage,
  fileSizeConver,
} from "@/utils/layoutUtil";
let Base64 = require('js-base64').Base64
export default {
  components: { HeaderEcharts },
  data() {
    return {
      exportCount:50,
      confirmLoading:false,
      columns: [
        // {
        //   title: "项目名称",
        //   dataIndex: "name",
        //   sorter: true,
        //   scopedSlots: { customRender: "name" },
        //   sortDirections: ["descend", "ascend"],
        //   width: "100px",
        // },
        // {
        //   title: "标签",
        //   dataIndex: "tags",
        // },
        // {
        //   title: "版本",
        //   dataIndex: "version",
        //   sortDirections: ["descend", "ascend"],
        //   sorter: true,
        // },
        // {
        //   title: "分类",
        //   dataIndex: "classifier",
        //   sorter: true,
        //   // scopedSlots: { customRender: "classifier" },
        //   sortDirections: ["descend", "ascend"],
        // },
        // {
        //   title: "上次导入物料清单",
        //   dataIndex: "lastBomImport",
        //   sorter: true,
        //   scopedSlots: { customRender: "lastBomImport" },
        //   sortDirections: ["descend", "ascend"],
        //   width: "230px",
        // },
        // {
        //   title: "物料清单格式",
        //   dataIndex: "lastBomImportFormat",
        //   sorter: true,
        //   scopedSlots: { customRender: "lastBomImportFormat" },

        //   sortDirections: ["descend", "ascend"],
        // },
        // {
        //   title: "风险评分",
        //   dataIndex: "lastInheritedRiskScore",
        //   sorter: true,
        //   sortDirections: ["descend", "ascend"],
        // },
        // // {
        // //   title: "违反政策",
        // //   dataIndex: "policy",
        // //   sorter: true,
        // //   sortDirections: ["descend", "ascend"],
        // // },
        // {
        //   title: "漏洞",
        //   dataIndex: "vulnerabilities",
        //   scopedSlots: { customRender: "vulnerabilities" },
        // },
        {
          title: "存储空间",
          i18nKey: 'Artifacts.StorageSpace',
          dataIndex: "storageId",
          scopedSlots: { customRender: "storageId" },
          width: 130,
        },
        {
          title: "所属仓库",
          i18nKey: 'Artifacts.OwnedWarehouse',
          dataIndex: "repositoryId",
          scopedSlots: { customRender: "repositoryId" },
          width: 160,
        },
        {
          title: "制品路径",
          i18nKey: 'Artifacts.ProductPath',
          dataIndex: "artifactPath",
          scopedSlots: { customRender: "artifactPath" },
          width: 550,
        },
        // {
        //   title: "创建时间",
        //   dataIndex: "created",
        //   sorter: true,
        //   sortDirections: ["descend", "ascend"],
        //   scopedSlots: { customRender: "created" },
        //   width: 200,
        // },
        {
          title: "最近使用时间",
          i18nKey: 'Artifacts.LastUsedTime',
          dataIndex: "lastUsed",
          sorter: true,
          scopedSlots: { customRender: "lastUsed" },
          width: 200,
        },
        {
          title: "下载次数",
          i18nKey: 'Artifacts.DownloadTimes',
          dataIndex: "downloadCount",
          sorter: true,
          scopedSlots: { customRender: "created" },
          width: 160,
        },
        {
          title: "制品大小",
          i18nKey: 'Artifacts.ProductSize',
          dataIndex: "sizeInBytes",
          sorter: true,
          scopedSlots: { customRender: "sizeInBytes" },
          width: 150,
        },
        {
          title: "漏洞",
          i18nKey: 'Artifacts.Vulnerability',
          dataIndex: "vulnerabilitiesCount",
          sorter: true,
          scopedSlots: { customRender: "vulnerabilitiesCount" },
          width: 200,
        },
      ],
      artifactsData: [],
      artifactsTableLoading: false,
      queryParams: {
        // page: 1,
        // limit: 10,
        // sortOrder: "",
        // sortName: "",
        // onlyRoot: true,
        // excludeInactive: false,
        // total: 0,
        safeLevel: "scanComplete",
        artifactName: null,
        metadataSearch: null,
        storageId: null,
        repositoryId: null,
        limit: 10,
        page: 1,
        total: 0,
        sortField: null,
        sortOrder: null,
        beginDate: null,
        endDate: null,
      },
      selectRepository: [],
      repositoryList: []
    };
  },
  computed: {
    i18nColumns() {
      return this.columns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
  },
  created() {
    this.queryOnScanTreeList()
    this.getData()
  },
  methods: {
    formatTimestamp,
    handleInput(event) {
      const value = event.target.value;
      const numericValue = parseInt(value, 10);
      if (isNaN(numericValue) || numericValue < 0 || numericValue > 1000) {
        this.exportCount = numericValue < 0 ? 0 : numericValue > 1000 ? 1000 : value;
      }
    },
    getExportData(){
      this.confirmLoading = true
      // this.$notification.open({
      //   class: 'ant-notification-success',
      //   message: this.$t('Artifacts.exporting'),
      // });
      const queryParams = JSON.parse(JSON.stringify(this.queryParams))
      queryParams.page = 1

      // 如果用户没有输入导出数量默认导出50条
      if(!this.exportCount){
        queryParams.limit = 50
      }else{
        queryParams.limit = parseInt(this.exportCount)
      }
      fql(queryParams).then((res) => {
        this.exportExcelFn(res.artifact)
      })
    },
    // 导出execl表格
    exportExcelFn(list){
      const artifactsData = list
      // 数据封装
      artifactsData.forEach(row => {
        if(row.layout.toLowerCase() !== "docker"){
          row.artifactPath = row.path
        }
        row.sizeInBytes = this.fileSizeConver(row.sizeInBytes)
        row.vulnerabilitiesCount = [
          {
            key: this.$t('Artifacts.Seriously'),
            value: row.criticalVulnerabilitiesCount,
            color:'f86c6b',
            bold:false
          },
          {
            key: this.$t('Artifacts.MediumRisk'),
            value: row.highVulnerabilitiesCount,
            color:'fd8c00',
            bold:false
          },
          {
            key: this.$t('Artifacts.HighRisk'),
            value: row.mediumVulnerabilitiesCount,
            color:'ffc107',
            bold:false
          },
          {
            key: this.$t('Artifacts.LowRisk'),
            value: row.lowVulnerabilitiesCount,
            color:'4dbd74',
            bold:false
          }
        ]
      })

      // 定义表头
      const headers = [
          { header: this.$t('Artifacts.StorageSpace'), key: 'storageId', width: 25 },
          { header: this.$t('Artifacts.OwnedWarehouse'), key: 'repositoryId', width: 25 },
          { header: this.$t('Artifacts.ProductPath'), key: 'artifactPath', width: 70 },
          { header: this.$t('Artifacts.LastUsedTime'), key: 'lastUsed', width: 22 },
          { header: this.$t('Artifacts.DownloadTimes'), key: 'downloadCount', width: 15 },
          { header: this.$t('Artifacts.ProductSize'), key: 'sizeInBytes', width: 15 },
          { header: this.$t('Artifacts.Vulnerability'), key: 'vulnerabilitiesCount', width: 35 },
      ]
      // 定义文件名称
      const fileName = `${this.$t('Artifacts.ProductScan')}.xlsx`
      this.$exportExcel(artifactsData, headers, fileName)
      this.confirmLoading = false
    },
    queryOnScanTreeList() {
      this.repositoryList = []
      queryOnScanTree().then((res) => {
        if (res) {
          this.repositoryList = res
        }
      })
    },
    repositoryChange(value) {
      if (value && value.length > 0) {
        this.selectRepository = value
        this.queryParams.storageId = value[0]
        this.queryParams.repositoryId = value[1]
      } else {
        this.selectRepository = []
        this.queryParams.storageId = null
        this.queryParams.repositoryId = null
      }
      this.queryParams.page = 1
      this.getData()
    },
    repositoryFilter(inputValue, path) {
      return path.some(option => option.label.toLowerCase().indexOf(inputValue.toLowerCase()) > -1);
    },
    // 获取表格数据
    fileSizeConver(size) {
      if (size) {
        return fileSizeConver(size)
      }
    },
    getData() {
      // getProjectsList(this.queryParams).then((res) => {
      //   this.queryParams.total = +res.headers["x-total-count"];
      //   this.projectsData = res.data;
      // });
      this.artifactsTableLoading = true
      fql(this.queryParams).then((res) => {
        this.artifactsData = res.artifact
        this.queryParams.total = res.total
      }).finally(() => {
        this.artifactsTableLoading = false
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.queryParams.page = pagination.current
      }
      this.queryParams.sortField = sorter.field
      if (sorter && sorter.order === "descend") {
        this.queryParams.sortOrder = "desc"
      } else if (sorter && sorter.order === "ascend") {
        this.queryParams.sortOrder = "asc"
      } else {
        this.queryParams.sortOrder = ""
      }
      this.getData()
    },
    handleGoDetail(row) {
      let data = JSON.stringify({
        storageId: row.storageId,
        repositoryId: row.repositoryId,
        artifactPath: row.layout.toLowerCase() === "docker" ? row.artifactPath : row.path,
        layout: row.layout
      })
      this.$router.push({
        path: '/artifacts/artifactsDetail',
        query: {
          data: data
        }
      })
    },
    handheTableSearch() {
      this.queryParams.page = 1
      this.getData()
    },
  },
};
</script>

<style lang="scss" scoped>
.search {
  height: 50px;
}

.mx-25 .ant-row-flex {
  flex-wrap: wrap;
}

.v-search {
  max-width: 200px;
  width: 170px;
  min-width: 150px;
  margin-left: 5px;
  margin-bottom: 8px;
}

.repository-query {
  min-width: 220px;
}

.repository-query::v-deep .ant-cascader-picker-label {
  padding: 0 30px 0 12px;
}

.export_excel_sty{
  background: #fff;
  width: 38px;
  height: 38px;
  padding: 9px;
  border-radius: 6px;
  box-shadow: 0px 1px 6px 2px rgba(0, 0, 0, 0.1);
  &:hover{
    box-shadow: 0px 1px 6px 2px rgba(0, 0, 0, 0.15);
    cursor: pointer;
  }
}
</style>
