<template>
  <div class="wrapper">
    <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search
            :placeholder="$t('Projects.ServicesNameQuery')"
            class="v-search"
            v-model="queryParams.searchText"
            @search="handheTableSearch()"
          />
        </a-col>
      </div>
      <a-table
        rowKey="uuid"
        class="mt-20"
        :columns="i18nColumns"
        :data-source="projectsData"
        @change="handleChangeTable"
        :pagination="{
          pageSize: queryParams.pageSize,
          current: queryParams.pageNumber,
          total: queryParams.total || 0,
          showLessItems: true,
        }"
      >
        <template slot="name" slot-scope="name, row">
          <a-button type="link" @click="handleGoDetail(row)">
            {{ name }}
          </a-button>
        </template>
        <template slot="license" slot-scope="license, row">
          <a-button type="link" @click="handleGoLicense(row)">
            {{ license }}
          </a-button>
        </template>
        <template slot="vulnerabilities" slot-scope="vulnerabilities, row">
          <a-tag color="#f86c6b">{{ row.metrics && row.metrics.critical ? row.metrics.critical : 0 }}</a-tag>
          <a-tag color="#fd8c00">{{ row.metrics && row.metrics.high ? row.metrics.high : 0 }}</a-tag>
          <a-tag color="#ffc107">{{ row.metrics && row.metrics.medium ? row.metrics.medium : 0 }}</a-tag>
          <a-tag color="#4dbd74"> {{ row.metrics && row.metrics.low ? row.metrics.low : 0 }}</a-tag>
          <a-tag color="#777777"> {{ row.metrics && row.metrics.unassigned ? row.metrics.unassigned : 0 }}</a-tag>
        </template>
      </a-table>
    </a-card>
  </div>
</template>

<script>
import { getProjectsService } from "@/api/projects.js"
import { formatTimestamp } from "@/utils/util.js"
export default {
  components: {},
  computed: {
    i18nColumns() {
      return this.columns.map((column) => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey)
        }
        return column
      })
    },
  },
  data() {
    return {
      columns: [
        {
          i18nKey: "Projects.Name",
          title: "名称",
          dataIndex: "name",
          sorter: true,
          scopedSlots: { customRender: "name" },
          sortDirections: ["descend", "ascend"],
        },
        {
          i18nKey: "Projects.Version",
          title: "版本",
          dataIndex: "version",
          sortDirections: ["descend", "ascend"],
          sorter: true,
          width: "130px",
        },

        {
          i18nKey: "Projects.Authenticated",
          title: "认证",
          dataIndex: "authenticated",
        },
        {
          i18nKey: "Projects.CrossTrustBoundary",
          title: "跨信任边界",
          dataIndex: "crossesTrustBoundary",
          scopedSlots: { customRender: "crossesTrustBoundary" },
          width: "200px",
        },
        {
          i18nKey: "Projects.RiskScore",
          title: "风险评分",
          dataIndex: "lastInheritedRiskScore",
          sorter: true,
          sortDirections: ["descend", "ascend"],
          width: "130px",
        },
        {
          i18nKey: "Projects.Vulnerabilities",
          title: "漏洞",
          dataIndex: "metrics",
          width: "130px",
          scopedSlots: { customRender: "vulnerabilities" },
        },
      ],
      projectsData: [],
      queryParams: {
        pageNumber: 1,
        pageSize: 10,
        total: 0,
        searchText: "",
      },
    }
  },
  created() {
    this.getData()
  },
  methods: {
    formatTimestamp,
    // 获取表格数据
    getData() {
      const uuid = this.$route.params.id
      getProjectsService(uuid, this.queryParams).then((res) => {
        this.queryParams.total = +res.headers["x-total-count"]
        this.projectsData = res.data === "" ? [] : res.data
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.queryParams.pageNumber = pagination.current
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
      this.$router.push(`/projectsDetail/${row.uuid}`)
    },
    handleGoLicense(row) {
      this.$router.push(`/projectsDetail/${row.uuid}`)
    },
    handheTableSearch() {
      this.queryParams.pageNumber = 1
      this.getData()
    },
  },
}
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
