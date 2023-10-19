<template>
  <div class="wrapper">
    <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
      <div class="mx-25 search">
        <a-col :span="24" class="text-right">
          <a-input-search placeholder="输入制品路径查询" class="v-search" v-model="queryParams.searchKeyword" @search="handheTableSearch()" />
        </a-col>
      </div>
      <a-table
        rowKey="uuid"
        class="mt-20"
        :columns="columns"
        :data-source="artifacts"
        @change="handleChangeTable"
        :scroll="{ x: true }"
        :loading="artifactsTableLoading"
        :pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }"
      >
        <template slot="artifactPath" slot-scope="artifactPath, row">
          <a-button type="link" @click="handleGoDetail(row)">
            {{ artifactPath }}
          </a-button>
        </template>
      </a-table>
    </a-card>
  </div>
</template>

<script>
import { getArtifact } from "@/api/module.js";
import { formatTimestamp } from "@/utils/util.js";
export default {
  props: {
    component: {
      type: Object,
      default: {},
    },
  },
	watch: {
    component: {
      handler(newVal, oldVal) {
        this.getData();
      },
      deep: true,
    },
  },
  components: {},
  data() {
    return {
      columns: [
        {
          title: "存储空间",
          dataIndex: "storageId",
        },
        {
          title: "所属仓库",
          dataIndex: "repositoryId",
        },
        {
          title: "制品路径",
          dataIndex: "artifactPath",
          scopedSlots: { customRender: "artifactPath" },
        },
      ],
      artifacts: [],
      artifactsTableLoading: false,
      queryParams: {
        page: 1,
        limit: 10,
        sortOrder: "",
        sortName: "",
        componentUuid: "",
        searchKeyword: "",
        total: 0,
      },
    };
  },
  created() {
    this.getData()
  },
  methods: {
    formatTimestamp,
    // 获取表格数据
    getData() {
      this.queryParams.componentUuid = this.component.uuid
      this.artifactsTableLoading = true
      getArtifact(this.queryParams).then((res) => {
        this.queryParams.total = res.data.total
        this.artifacts = res.data.rows
      }).finally(() => {
        this.artifactsTableLoading = false
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
      this.getData();
    },
    handleGoDetail(row) {
      let data = JSON.stringify({
        storageId: row.storageId,
        repositoryId: row.repositoryId,
        artifactPath: row.artifactPath,
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
</style>
