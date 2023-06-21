<template>
  <div class="wrapper">
    <!-- <HeaderEcharts></HeaderEcharts> -->
    <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px; overflow-y: auto" class="header-solid" >
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search placeholder="输入制品路径查询" class="v-search" v-model="queryParams.artifactName" @search="handheTableSearch()" />
        </a-col>
      </div>
      <a-table
        rowKey="uuid"
        class="mt-20"
        :columns="columns"
        :data-source="artifactsData"
        @change="handleChangeTable"
        :scroll="{ x: true }"
        :pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }"
      >
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
          <a-tag color="#4dbd74"> {{ row.lowVulnerabilitiesCount}}</a-tag>
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
          dataIndex: "storageId",
          scopedSlots: { customRender: "storageId" },
          width: 130,
        },
        {
          title: "所属仓库",
          dataIndex: "repositoryId",
          scopedSlots: { customRender: "repositoryId" },
          width: 130,
        },
        {
          title: "制品路径",
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
          dataIndex: "lastUsed",
          sorter: true,
          scopedSlots: { customRender: "lastUsed" },
          width: 200,
        },
        {
          title: "下载次数",
          dataIndex: "downloadCount",
          sorter: true,
          scopedSlots: { customRender: "created" },
          width: 120,
        },
        {
          title: "制品大小",
          dataIndex: "sizeInBytes",
          sorter: true,
          scopedSlots: { customRender: "sizeInBytes" },
          width: 120,
        },
        {
          title: "漏洞",
          dataIndex: "vulnerabilitiesCount",
          sorter: true,
          scopedSlots: { customRender: "vulnerabilitiesCount" },
          width: 200,
        },
      ],
      artifactsData: [],
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
        limit: 5,
        page: 1,
        total: 0,
        sortField: null,
        sortOrder: null,
        beginDate: null,
        endDate: null,
      },
    };
  },
  created() {
    this.getData();
  },
  methods: {
    formatTimestamp,
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
      fql(this.queryParams).then((res) => {
        this.artifactsData = res.artifact
        this.queryParams.total = res.total
      }).finally(() => {
        this.loading = false
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.queryParams.page = pagination.current;
      }
      this.queryParams.sortField = sorter.field;
      if (sorter && sorter.order === "descend") {
        this.queryParams.sortOrder = "desc";
      } else if (sorter && sorter.order === "ascend") {
        this.queryParams.sortOrder = "asc";
      } else {
        this.queryParams.sortOrder = "";
      }
      this.getData();
    },
    handleGoDetail(row) {
      let data = JSON.stringify({
        storageId: row.storageId,
        repositoryId: row.repositoryId,
        artifactPath: row.layout.toLowerCase() === "docker" ?row.artifactPath:row.path,
        layout: row.layout
      })
      this.$router.push({ 
        path: '/artifactsDetail',
        query: {
          data: data
        }
      });
    },
    handheTableSearch() {
      this.queryParams.page = 1;
      this.getData();
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
</style>
