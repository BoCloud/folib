<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-card :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
          :title="$t('CustomLayout.CustomLayoutList')">
          <div slot="extra">
            <a-tooltip @click="showCustomLayoutInfo(null)">
              <template slot="title">{{ $t('Setting.Add') }}</template>
              <a-icon type="plus-circle" theme="filled" class="cursor-pointer custom-layout-add mr-10"
                :style="{ fontSize: '28px', color: '#1890FF' }" />
            </a-tooltip>
            <a-input-search :placeholder="$t('CustomLayout.EnterCustomLayoutNameQuery')" class="v-search"
              v-model="customLayoutQuery.matchLayoutName" @search="handheSearch()" />
          </div>
          <a-table :columns="i18nCustomLayoutColumns" :data-source="customLayoutList" :scroll="{ x: true }"
            @change="handleChangeTable" :loading="customLayoutLoading"
            :pagination="{ pageSize: customLayoutQuery.limit, current: customLayoutQuery.page, total: customLayoutQuery.total, showLessItems: true }"
            :row-key="(r, i) => i.toString()">
            <template slot="layoutName" slot-scope="layoutName">
              <span>
                {{ layoutName }}
              </span>
            </template>
            <div slot="operation" slot-scope="text, record">
              <div class="col-action">
                <a-popconfirm :title="$t('Package.SureDelete')" okType="danger" :ok-text="$t('Package.Confirm')"
                  :cancel-text="$t('Package.Cancel')" @confirm="customLayoutDelete(record)">
                  <a-button type="link" size="small">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                        fill="#111827" />
                    </svg>
                    <span class="text-danger">DELETE</span>
                  </a-button>
                </a-popconfirm>
                <a-button type="link" size="small" @click="showCustomLayoutInfo(record)">
                  <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path class="fill-muted"
                      d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                      fill="#111827" />
                    <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                      fill="#111827" />
                  </svg>
                  <span class="text-dark">EDIT</span>
                </a-button>
              </div>
            </div>
          </a-table>
        </a-card>
      </a-col>
    </a-row>
    <a-drawer placement="right" width="60%" :title="$t('CustomLayout.CustomLayoutSetting')"
      v-if="customLayoutVisible" :visible="customLayoutVisible" @close="customLayoutClose" :zIndex="100">
      <a-card :bordered="false" class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
        <a-form-model layout="horizontal" ref="customLayoutFormRef" :model="customLayoutForm"
          :rules="customLayoutRules" :hideRequiredMark="false">
          <a-form-model-item class="mb-10" :label="$t('CustomLayout.LayoutName')" :colon="false"
            prop="layoutName">
            <a-input :placeholder="$t('CustomLayout.EnterLayoutName')"
              :disabled="customLayoutForm.id != null"
              v-model="customLayoutForm.layoutName" class="custom-layout-form-common" />
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="artifactPathPattern">
            <template slot="label">
              {{ $t('CustomLayout.ArtifactPathPattern') }}
            </template>
            <a-input :placeholder="$t('CustomLayout.EnterArtifactPathPattern')" v-model="customLayoutForm.artifactPathPattern" type="textarea" :rows="3" class="custom-layout-form-common" />
          </a-form-model-item>
          <a-form-model-item :wrapper-col="{ span: 14, offset: 4 }">
            <a-button type="primary" @click="customLayoutFormSubmit">
              {{ $t('Setting.Save') }}
            </a-button>
            <a-button class="ml-10" @click="customLayoutClose">
              {{ $t('Setting.Cancel') }}
            </a-button>
          </a-form-model-item>
        </a-form-model>
      </a-card>
    </a-drawer>
  </div>
</template>
<script>
import {
  queryCustomLayout,
  getCustomLayout,
  saveCustomLayout,
  updateCustomLayout,
  deleteCustomLayout,
} from "@/api/customLayout"

export default {
  props: {
    securityPolicyActiveKey: {
      type: String,
      default: "",
    },
  },
  data() {
    const checkLayoutName = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('CustomLayout.EnterLayoutName')))
      } else {
        callback()
      }
    }
    const checkArtifactPathPattern = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('CustomLayout.EnterArtifactPathPattern')))
      } else {
        callback()
      }
    }
    return {
      customLayoutVisible: false,
      customLayoutForm: {
        id: null,
        layoutName: '',
        artifactPathPattern: '',
      },
      directoryPaths: [],
      customLayoutRules: {
        layoutName: [
          { required: true, trigger: ['blur'], validator: checkLayoutName },
        ],
        artifactPathPattern: [
          { required: true, trigger: ['blur', 'change'], validator: checkArtifactPathPattern },
        ],
      },
      customLayoutList: [],
      customLayoutQuery: {
        page: 1,
        limit: 10,
        total: 0,
        matchLayoutName: undefined,
        storageId: undefined,
        repositoryId: undefined,
      },
      customLayoutLoading: false,
      customLayoutColumns: [
        {
          title: '布局名称',
          i18nKey: 'CustomLayout.LayoutName',
          dataIndex: 'layoutName',
          key: 'layoutName',
          width: 150,
          scopedSlots: { customRender: 'layoutName' },
        },
        {
          title: '制品路径正则表达式',
          i18nKey: 'CustomLayout.ArtifactPathPattern',
          dataIndex: 'artifactPathPattern',
          key: 'artifactPathPattern',
          width: 300,
          customCell: () => {
            return {
              style: {
                maxWidth: '300px',
                overflow: 'hidden',
                whiteSpace: 'nowrap',
                textOverflow: 'ellipsis',
                cursor: 'pointer'
              }
            }
          },
          customRender: (text, record) => <a-tooltip placement="topLeft" title={record.artifactPathPattern} >{record.artifactPathPattern}</a-tooltip>
        },
        {
          title: '操作',
          i18nKey: 'CustomLayout.Operate',
          dataIndex: 'operation',
          width: 220,
          scopedSlots: { customRender: 'operation' },
        },
      ],
    }
  },
  components: {
  },
  computed: {
    i18nCustomLayoutColumns() {
      return this.customLayoutColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    },
  },
  created() {
    this.initData()
  },
  watch: {
    'securityPolicyActiveKey': function (newval) {
      if (newval === "3") {
        this.initData()
      }
    },
  },
  mounted() { },
  methods: {
    highlighterHandle(code) {
      return highlight(code, languages.js)
    },
    successMsg(message) {
      if (!message) {
        message = this.$t('CustomLayout.OperationSuccessful');
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    initData() {
      this.queryCustomLayout()
    },
    queryCustomLayout() {
      this.customLayoutLoading = true
      queryCustomLayout(this.customLayoutQuery).then(res => {
        this.customLayoutList = []
        if (res && res.data) {
          this.customLayoutList = res.data.rows
          this.customLayoutQuery.total = res.data.total
        }
      }).finally(() => {
        this.customLayoutLoading = false
      })
    },
    getCustomLayout(customLayout) {
      getCustomLayout({ layoutName: customLayout.layoutName }).then(res => {
        if (res) {
          this.customLayoutForm.id = customLayout.id
          this.customLayoutForm.layoutName = res.layoutName
          this.customLayoutForm.artifactPathPattern = res.artifactPathPattern
        }
      }).finally(() => {
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.customLayoutQuery.page = pagination.current
      }
      this.queryCustomLayout()
    },
    handheSearch() {
      this.customLayoutQuery.page = 1
      this.queryCustomLayout()
    },
    customLayoutDelete(item) {
      deleteCustomLayout({ layoutName: item.layoutName }).then(res => {
        this.successMsg(this.$t('CustomLayout.OperationSuccessful'))
        this.queryCustomLayout()
      }).catch((err) => {
        let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
        if (msg && msg.length > 0) {
          this.$notification.error({
            message: msg,
            description: ""
          })
        }
      })
    },
    showCustomLayoutInfo(item) {
      this.customLayoutResetForm()
      if (item) {
        this.getCustomLayout(item)
      }
      this.customLayoutVisible = true
    },
    customLayoutClose() {
      this.customLayoutResetForm()
      this.customLayoutVisible = false
    },
    customLayoutFormSubmit() {
      this.$refs.customLayoutFormRef.validate(valid => {
        if (valid) {
          let dataForm = Object.assign({}, this.customLayoutForm)
          if (dataForm.id) {
            updateCustomLayout(dataForm).then(res => {
              this.successMsg(this.$t('CustomLayout.OperationSuccessful'))
              this.queryCustomLayout()
              this.customLayoutClose()
            }).catch((err) => {
              let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
              if (msg && msg.length > 0) {
                this.$notification.error({
                  message: msg,
                  description: ""
                })
              }
            })
          } else {
            saveCustomLayout(dataForm).then(res => {
              this.successMsg(this.$t('CustomLayout.OperationSuccessful'))
              this.queryCustomLayout()
              this.customLayoutClose()
            }).catch((err) => {
              let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
              if (msg && msg.length > 0) {
                this.$notification.error({
                  message: msg,
                  description: ""
                })
              }
            })
          }
        }
      })
    },
    customLayoutResetForm() {
      this.customLayoutForm = {
        id: null,
        layoutName: '',
        artifactPathPattern: '',
      }
      if (this.$refs.customLayoutFormRef) {
        this.$refs.customLayoutFormRef.resetFields()
      }
    },
  },
};
</script>

<style lang="scss" scoped>
.v-search {
  max-width: 200px;
  width: 170px;
  min-width: 150px;
  margin-left: 5px;
  margin-bottom: 8px;
}
.custom-layout-add {
  vertical-align: middle;
}
.custom-layout-form-common {
  width: 100%
}
</style>
