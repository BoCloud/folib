<template>
  <a-card class="header-solid package-name">
    <div class="mx-25 search">
			<a-col :span="24" class="text-right">
				<a-input-search placeholder="输入包名称查询" class="v-search"
					v-model="packageNameQuery.packageName" @search="handleSearch()" />
			</a-col>
		</div>
    <a-table :columns="packageNameColumns" :data-source="packageNameList" :scroll="{ x: true }"
      @change="handleChangeTable"
      :loading="loading"
      :pagination="{ pageSize: packageNameQuery.limit, current: packageNameQuery.page, total: packageNameQuery.total, showLessItems: true }"
      :row-key="(r, i) => i.toString()">
      <div slot="operation" slot-scope="text, record">
        <div class="col-action">
          <a-popconfirm title="确定要删除吗？" okType="danger" ok-text="确定" cancel-text="取消"
            @confirm="packageNameHandlerDelete(record)">
            <a-button type="link" size="small">
              <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                  d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                  fill="#111827" />
              </svg>
              <span class="text-danger">DELETE</span>
            </a-button>
          </a-popconfirm>
        </div>
      </div>
    </a-table>
  </a-card>
</template>
<script>
import {
  getPackageNameBlock,
  deletePackageNameBlock
} from '@/api/packageNameBlock'
export default {
  props: [

  ],
  components: {
  },
  data() {
    return {
      packageNameColumns: [
        {
          title: '包名',
          dataIndex: 'packageName',
          key: 'packageName',
          width: 150,
          scopedSlots: { customRender: 'packageName' },
        },
        {
          title: '条件类型',
          dataIndex: 'conditionValue',
          key: 'conditionValue',
          width: 150,
          scopedSlots: { customRender: 'conditionValue' },
        },
        {
          title: '版本号',
          dataIndex: 'version',
          key: 'version',
          width: 150,
          scopedSlots: { customRender: 'version' },
        },
        {
          title: '操作',
          dataIndex: 'operation',
          width: 100,
          scopedSlots: { customRender: 'operation' },
        },
      ],
      packageNameQuery: {
        page: 1,
        limit: 5,
        total: 0,
        packageName: undefined
      },
      loading: false,
      packageNameList: [],
    }
  },
  created() {
    this.getPackageNameList()
  },
  mounted() { },
  methods: {
    successMsg(message) {
      if (!message) {
        message = "操作成功"
      }
      this.$notification["success"]({
        message: message,
      })
    },
    getPackageNameList() {
      this.loading = true
      getPackageNameBlock(this.packageNameQuery).then(res => {
        this.packageNameList = []
        if (res && res.data) {
          this.packageNameList = res.data.rows
          this.packageNameQuery.total = res.data.total
        }
      }).finally(() => {
        this.loading = false
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.packageNameQuery.page = pagination.current
      }
      this.getPackageNameList()
    },
    handleSearch() {
      this.packageNameQuery.page = 1
      this.getPackageNameList()
    },
    packageNameHandlerDelete(record) {
      deletePackageNameBlock({id: record.id}).then(res => {
        this.successMsg('删除包名 ' + record.packageName + ' 成功')
      }).catch(err => {
        this.$notification['error']({
          message: err.response.data.error,
          description: ''
        })
      }).finally(() => {
        this.handleSearch()
      })
    },
  },
}
</script>

<style lang="scss" scoped>
$md: 768px;

.package-name::v-deep {
	.v-search {
		max-width: 200px;
		width: 170px;
		min-width: 150px;
		margin-left: 5px;
		margin-bottom: 8px;
	}

	.v-search-div {
		display: inline-block;
	}

	.mx-25 .ant-row-flex {
		flex-wrap: wrap;
	}

	.search {
		height: 50px;
	}
}
</style>