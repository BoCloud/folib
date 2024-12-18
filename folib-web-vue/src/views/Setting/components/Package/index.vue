<template>
  <div class="package-name">
    <a-card class="header-solid" :bordered="false" :title="$t('Setting.PackageNameBlacklist')">
      <div slot="extra">
        <a-tooltip @click="packageNameModalShow">
          <template slot="title">{{ $t('Setting.Add') }}</template>
          <a-icon type="plus-circle" theme="filled" class="cursor-pointer package-name-add mr-20"
            :style="{ fontSize: '28px', color: '#1890FF' }" />
        </a-tooltip>
        <a-input-search :placeholder="$t('Package.PackageNameQuery')" class="v-search"
          v-model="packageNameQuery.packageName" @search="handleSearch()" />
      </div>
      <a-table :columns="i18nPackageNameColumns" :data-source="packageNameList" :scroll="{ x: true }"
        @change="handleChangeTable" :loading="loading"
        :pagination="{ pageSize: packageNameQuery.limit, current: packageNameQuery.page, total: packageNameQuery.total, showLessItems: true }"
        :row-key="(r, i) => i.toString()">
        <div slot="operation" slot-scope="text, record">
          <div class="col-action">
            <a-popconfirm :title="$t('Package.SureDelete')" okType="danger" :ok-text="$t('Package.Confirm')"
              :cancel-text="$t('Package.Cancel')" @confirm="packageNameHandlerDelete(record)">
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
      <AddPackageName v-if="showPackageNameModal" :modelVisible="showPackageNameModal"
        @packageNameHandlerCancel="packageNameModalCancel" @packageNameRefresh="packageNameRefresh" />
    </a-card>
  </div>
</template>
<script>
import {
  getPackageNameBlock,
  deletePackageNameBlock
} from '@/api/packageNameBlock'
import AddPackageName from "./add"
export default {
  props: [

  ],
  components: {
    AddPackageName,
  },
  computed: {
    i18nPackageNameColumns() {
      return this.packageNameColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
  },
  data() {
    return {
      packageNameColumns: [
        {
          title: '包名称',
          i18nKey: 'Package.PackageName',
          dataIndex: 'packageName',
          key: 'packageName',
          // width: 500,
          scopedSlots: { customRender: 'packageName' },
        },
        {
          title: '条件类型',
          i18nKey: 'Package.ConditionType',
          dataIndex: 'conditionValue',
          key: 'conditionValue',
          // width: 200,
          scopedSlots: { customRender: 'conditionValue' },
        },
        {
          title: '版本号',
          i18nKey: 'Package.Version',
          dataIndex: 'version',
          key: 'version',
          // width: 200,
          scopedSlots: { customRender: 'version' },
        },
        {
          title: '操作',
          i18nKey: 'Package.Operate',
          dataIndex: 'operation',
          width: 80,
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
      showPackageNameModal: false,
    }
  },
  created() {
    this.getPackageNameList()
  },
  mounted() { },
  methods: {
    successMsg(message) {
      if (!message) {
        message = this.$t("Package.OperateSuccess")
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
      deletePackageNameBlock(record).then(res => {
        this.successMsg(this.$t('Package.DeletePackage') + ' ' + record.packageName + this.$t('Package.Success'))
      }).catch(err => {
        this.$notification['error']({
          message: err.response.data.error,
          description: ''
        })
      }).finally(() => {
        this.handleSearch()
      })
    },
    packageNameModalCancel() {
      this.showPackageNameModal = false
    },
    packageNameModalShow() {
      this.showPackageNameModal = true
    },
    packageNameRefresh() {
      this.getPackageNameList()
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