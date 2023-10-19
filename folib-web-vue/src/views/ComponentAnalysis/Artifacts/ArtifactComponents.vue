<template>
  <div class="wrapper">
    <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search placeholder="输入组件名称查询" class="v-search" v-model="queryParams.searchKeyword" @search="handheTableSearch()" />
        </a-col>
      </div>
      <a-table
        rowKey="uuid"
        class="mt-20"
        :columns="columns"
        :data-source="componentsData"
        @change="handleChangeTable"
        :scroll="{ x: true }"
        :loading="componentsTableLoading"
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
import { getProjectsComponentsByArtifact } from "@/api/projects.js";
import { formatTimestamp } from "@/utils/util.js";
export default {
  props: {
    artifactData: {
      type: Object,
      default: {},
    },
  },
  components: {},
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
          width: "200px",
        },
        {
          title: "组",
          dataIndex: "groupId",
        }, 
        {
          title: "许可证",
          dataIndex: "license",
          scopedSlots: { customRender: "license" },
          width: "200px",
        },
        {
          title: "漏洞",
          dataIndex: "vulnerabilitiesCount",
          scopedSlots: { customRender: "vulnerabilitiesCount" },
          width: "200px",
        },
      ],
      componentsData: [],
      componentsTableLoading: false,
      queryParams: {
        page: 1,
        limit: 10,
        total: 0,
        artifactPath: "",
        searchKeyword: "",
      },
    };
  },
  watch: {
    artifactData() {
      this.getData()
    },
    deep: true,
  },
  created() {
    this.getData()
  },
  methods: {
    formatTimestamp,
    // 获取表格数据
    getData() {
      if (!this.artifactData.artifact.uuid) {
        return
      }
      this.queryParams.artifactPath = this.artifactData.artifact.uuid
      this.componentsTableLoading = true
      getProjectsComponentsByArtifact(this.queryParams).then((res) => {
        this.queryParams.total = res.data.total
        this.componentsData = res.data.rows
      }).finally(() => {
        this.componentsTableLoading = false
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.queryParams.page = pagination.current
      }
      this.queryParams.sortName = sorter.field
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
      this.$router.push(`/components/componentsDetail/${row.uuid}`)
    },
    handleGoLicense(licenseId) {
      this.$router.push(`/licenses/licensesDetail/${licenseId}`)
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
</style>
