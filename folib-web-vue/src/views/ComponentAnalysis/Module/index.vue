<template>
  <div class="wrapper">
    <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20p; overflow-y: auto">
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search placeholder="输入组名称查询" class="v-search" v-model="queryParams.groupId" @search="handheTableSearch()" />
          <a-input-search placeholder="输入组件名称查询" class="v-search" v-model="queryParams.name" @search="handheTableSearch()" />
          <a-input-search placeholder="输入版本查询" class="v-search" v-model="queryParams.version" @search="handheTableSearch()" />
        </a-col>
      </div>
      <a-table
        rowKey="uuid"
        class="mt-20"
        :columns="columns"
        :data-source="projectsData"
        @change="handleChangeTable"
        :scroll="{ x: true }"
        :pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }"
      >
        <template slot="name" slot-scope="name, row">
          <a-button type="link" @click="handleGoDetail(row)">
            {{ name }}
          </a-button>
        </template>
        <template slot="license" slot-scope="license, row">
          <span v-for="(item, index) in row.license" :key="index" class="mr-5">
            <a-button type="link" @click="handleGoLicense(item)">
              {{ item }}
            </a-button>
          </span>
        </template>
        <template slot="vulnerabilitiesCount" slot-scope="vulnerabilitiesCount, row">
          <a-tag color="#f86c6b">{{ row.criticalVulnerabilitiesCount }} </a-tag>
          <a-tag color="#fd8c00">{{ row.highVulnerabilitiesCount }} </a-tag>
          <a-tag color="#ffc107">{{ row.mediumVulnerabilitiesCount }} </a-tag>
          <a-tag color="#4dbd74"> {{ row.lowVulnerabilitiesCount}}</a-tag>
        </template>
      </a-table>
    </a-card>
  </div>
</template>

<script>
import { getComponentsList } from "@/api/module.js";
import { formatTimestamp } from "@/utils/util.js";
export default {
  components: {  },
  data() {
    return {
      columns: [
        {
          title: "组件名称",
          dataIndex: "name",
          scopedSlots: { customRender: "name" },
        },
        {
          title: "版本",
          dataIndex: "version",
          width: "100px",
        },
        {
          title: "组",
          dataIndex: "groupId",
          scopedSlots: { customRender: "groupId" },
        },
        {
          title: "许可证",
          dataIndex: "license",
          scopedSlots: { customRender: "license" },
          width: "200px",
        },
        {
          title: "PURL",
          dataIndex: "purl",
          scopedSlots: { customRender: "purl" },
          width: "500px",
        },
        {
          title: "漏洞",
          dataIndex: "vulnerabilitiesCount",
          scopedSlots: { customRender: "vulnerabilitiesCount" },
          width: "200px",
        },
      ],
      projectsData: [],
      queryParams: {
        page: 1,
        limit: 10,
        sortOrder: "",
        sortName: "",
        groupId: "",
        name: "",
        version: "",
        total: 0,
      },
    };
  },
  created() {
    this.getData();
  },
  methods: {
    formatTimestamp,
    // 获取表格数据
    getData() {
      getComponentsList(this.queryParams).then((res) => {
        this.queryParams.total = res.data.total;
        this.projectsData = res.data.rows;
      });
    },
    handleChangeTable(pagination, filters, sorter) {
      console.log(pagination, ".......pagination");
      if (pagination) {
        this.queryParams.page = pagination.current;
      }
      this.queryParams.sortName = sorter.field;
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
      this.$router.push(`/componentsDetail/${row.uuid}`);
    },
    handleGoProject(row) {
      this.$router.push(`/artifactsDetail/${row.project.uuid}`);
    },
    handleGoLicense(item) {
      this.$router.push(`/licensesDetail/${item}`);
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
