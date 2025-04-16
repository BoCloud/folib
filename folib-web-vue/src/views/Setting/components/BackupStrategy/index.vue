<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-card :bordered="false" class="header-solid" :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
          :title="$t('BackupStrategy.StrategyList')">
          <div slot="extra">
            <a-tooltip @click="showBackupStrategyInfo(null)">
              <template slot="title">{{ $t('Setting.Add') }}</template>
              <a-icon type="plus-circle" theme="filled" class="cursor-pointer backup-strategy-add mr-10"
                :style="{ fontSize: '28px', color: '#1890FF' }" />
            </a-tooltip>
            <a-input-search :placeholder="$t('BackupStrategy.EnterBackupStrategyNameQuery')" class="v-search"
              v-model="backupStrategyQuery.strategyName" @search="handheSearch()" />
            <a-input-search :placeholder="$t('BackupStrategy.EnterStorageIdQuery')" class="v-search"
              v-model="backupStrategyQuery.storageId" @search="handheSearch()" />
            <a-input-search :placeholder="$t('BackupStrategy.EnterRepositoryIdQuery')" class="v-search"
              v-model="backupStrategyQuery.repositoryId" @search="handheSearch()" />
          </div>
          <a-table :columns="i18nBackupStrategyColumns" :data-source="backupStrategyList" :scroll="{ x: true }"
            @change="handleChangeTable" :loading="backupStrategyLoading"
            :pagination="{ pageSize: backupStrategyQuery.limit, current: backupStrategyQuery.page, total: backupStrategyQuery.total, showLessItems: true }"
            :row-key="(r, i) => i.toString()">
            <template slot="strategyName" slot-scope="strategyName">
              <span>
                {{ strategyName }}
              </span>
            </template>
            <template slot="repositories" slot-scope="text, record">
              <span v-if="record.backupStrategyRepositories" @click="showBackupStrategyRepositories(record)">
                {{record.backupStrategyRepositories.length + ' ' + $t('BackupStrategy.Total')}} 
                |
                <span v-for="(item, i) in record.backupStrategyRepositories" :key="i">
                  {{ item.storageId + ':' + item.repositoryId}}
                  <span v-if="i!=record.backupStrategyRepositories.length-1">,</span>
                </span>
              </span>
            </template>
            <template slot="cronExpression" slot-scope="cronExpression">
              <span>{{cronExpression}}</span>
            </template>
            <template slot="enabled" slot-scope="enabled">
              <span v-if="enabled"><a-tag color="green">{{ $t('BackupStrategy.YES') }}</a-tag></span>
              <span v-else><a-tag color="orange">{{ $t('BackupStrategy.NO') }}</a-tag></span>
            </template>
            <template slot="incremental" slot-scope="incremental">
              <span v-if="incremental"><a-tag color="green">{{ $t('BackupStrategy.YES') }}</a-tag></span>
              <span v-else><a-tag color="orange">{{ $t('BackupStrategy.NO') }}</a-tag></span>
            </template>
            <div slot="operation" slot-scope="text, record">
              <div class="col-action">
                <a-popconfirm :title="$t('Package.SureDelete')" okType="danger" :ok-text="$t('Package.Confirm')"
                  :cancel-text="$t('Package.Cancel')" @confirm="backupStrategyDelete(record)">
                  <a-button type="link" size="small">
                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                        fill="#111827" />
                    </svg>
                    <span class="text-danger">DELETE</span>
                  </a-button>
                </a-popconfirm>
                <a-button type="link" size="small" @click="showBackupStrategyInfo(record)">
                  <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path class="fill-muted"
                      d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                      fill="#111827" />
                    <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                      fill="#111827" />
                  </svg>
                  <span class="text-dark">EDIT</span>
                </a-button>
                <a-button type="link" size="small" @click="execute(record)">
                  <a-icon type="play-circle" theme="twoTone" />
                  <span class="text-dark">EXECUTE</span>
                </a-button>
              </div>
            </div>
          </a-table>
        </a-card>
      </a-col>
    </a-row>
    <a-drawer placement="right" width="60%" :title="$t('BackupStrategy.BackupStrategySetting')"
      v-if="backupStrategyVisible" :visible="backupStrategyVisible" @close="backupStrategyClose" :zIndex="100">
      <a-card :bordered="false" class="header-solid h-full card-profile-information"
        :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
        <a-form-model layout="horizontal" ref="backupStrategyFormRef" :model="backupStrategyForm"
          :rules="backupStrategyRules" :hideRequiredMark="false">
          <a-form-model-item class="mb-10" :label="$t('BackupStrategy.StrategyName')" :colon="false"
            prop="strategyName">
            <a-input :placeholder="$t('BackupStrategy.EnterStrategyName')"
              :disabled="backupStrategyForm.id != null"
              v-model="backupStrategyForm.strategyName" class="backup-form-common" />
          </a-form-model-item>
          <a-form-model-item class="mb-10" :label="$t('BackupStrategy.ChooseBackupRepository')" :colon="false"
            prop="repositoriesKeys">
            <a-transfer :data-source="repositoriesList"
              :titles="[$t('BackupStrategy.SelectableRepositories'), $t('BackupStrategy.SelectedRepositories')]"
              :render="item => item.title" :target-keys="selectedRepositoriesKeys" :disabled="false" show-search
              :filter-option="(inputValue, item) => item.key.indexOf(inputValue) !== -1" show-select-all
              @change="repositoriesOnChange" @scroll="handleRepositoriesScroll"
              :list-style="{ width: '450px', height: '350px', }">
            </a-transfer>
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="cronExpression">
            <template slot="label">
              {{ $t('BackupStrategy.CronExpression') }}
            </template>
            <a-input :placeholder="$t('BackupStrategy.EnterCronExpression')" v-model="backupStrategyForm.cronExpression" class="backup-form-common" />
          </a-form-model-item>
          <a-form-model-item class="mb-10" :label="$t('BackupStrategy.BackupPath')" :colon="false" prop="backupPath">
            <a-tree-select v-model="backupStrategyForm.backupPath" class="backup-form-common" :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }"
              :tree-data="directoryPaths" :placeholder="$t('BackupStrategy.ChooseBackupPath')" :load-data="onLoadData" />
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="incremental">
            <template slot="label">
              {{ $t('BackupStrategy.Incremental') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <div class="mb-0" :style="{width: '300px'}">{{ $t('BackupStrategy.IncrementalTip') }}</div>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>
            <a-switch v-model="backupStrategyForm.incremental" @change="incrementalChange"/>
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="retentionPeriod">
            <template slot="label">
              {{ $t('BackupStrategy.RetentionPeriod') }}
              <a-popover placement="topLeft">
                <template slot="content">
                  <div class="mb-0" :style="{width: '300px'}">{{ $t('BackupStrategy.RetentionPeriodTip') }}</div>
                </template>
                <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
              </a-popover>
            </template>
            <a-input-number :min="0" :formatter="value => `${value.split('.')[0]}`" v-model="backupStrategyForm.retentionPeriod" :disabled="retentionPeriodDisabled" class="backup-form-common"/>
          </a-form-model-item>
          <a-form-model-item class="mb-10" :colon="false" prop="enabled">
            <template slot="label">
              {{ $t('BackupStrategy.Enabled') }}
            </template>
            <a-switch v-model="backupStrategyForm.enabled"/>
          </a-form-model-item>
          <a-form-model-item :wrapper-col="{ span: 14, offset: 4 }">
            <a-button type="primary" @click="backupStrategyFormSubmit">
              {{ $t('Setting.Save') }}
            </a-button>
            <a-button class="ml-10" @click="backupStrategyClose">
              {{ $t('Setting.Cancel') }}
            </a-button>
          </a-form-model-item>
        </a-form-model>
      </a-card>
    </a-drawer>
    <a-modal v-model="showRepositoriesModal" :footer="null" :forceRender="true" on-ok="showRepositoriesModal = false">
      <a-list item-layout="vertical" size="large" :data-source="backupStrategyRepositories"
			:pagination="backupStrategyRepositories.length === 0 ? false : { pageSize: 8, total: backupStrategyRepositories.length, showLessItems: true, showTotal:total =>  ` ${total} ` + ' '+ $t('BackupStrategy.Total')}">
        <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
          <div>
            <a-tag class="mb-5 bg-warning">{{$t('BackupStrategy.StorageId')}}</a-tag>
            <span>{{ item.storageId }}</span>
          </div>
          <div>
            <a-tag class="mb-5 bg-success">{{$t('BackupStrategy.RepositoryId')}}</a-tag>
            <span>{{ item.repositoryId }}</span>
          </div>
        </a-list-item>
      </a-list>
    </a-modal>
  </div>
</template>
<script>
import {
  queryBackupStrategy,
  getBackupStrategy,
  saveBackupStrategy,
  updateBackupStrategy,
  deleteBackupStrategy,
  executeBackup,
} from "@/api/backupStrategy"
import {
  queryRepositories
} from "@/api/folib"
import {
  folderList
} from "@/api/advanced"
import cronstrue from 'cronstrue';

export default {
  props: {
    securityPolicyActiveKey: {
      type: String,
      default: "",
    },
  },
  data() {
    const checkStrategyName = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('BackupStrategy.EnterStrategyName')))
      } else {
        callback()
      }
    }
    const checkCronExpression = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('BackupStrategy.EnterCronExpression')))
      } else {
        let isOk = true
        try {
          cronstrue.toString(value)
        } catch (e) {
          isOk = false
        }
        if (!isOk) callback(new Error(this.$t('BackupStrategy.EnterCorrectCronExpression')))
        else callback()
      }
    }
    const checkBackupPath = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('BackupStrategy.ChooseBackupPath')))
      } else {
        callback()
      }
    }
    const checkRepositoriesKeys = (rule, value, callback) => {
      if (!this.selectedRepositoriesKeys || this.selectedRepositoriesKeys.length <= 0) {
        callback(new Error(this.$t('BackupStrategy.ChooseBackupRepository')))
      } else {
        callback()
      }
    }
    return {
      backupStrategyVisible: false,
      retentionPeriodDisabled: false,
      backupStrategyForm: {
        id: null,
        strategyName: '',
        cronExpression: '',
        enabled: true,
        backupPath: null,
        incremental: false,
        retentionPeriod: 7,
        repositories: [],
      },
      directoryPaths: [],
      backupStrategyRules: {
        strategyName: [
          { required: true, trigger: ['blur'], validator: checkStrategyName },
        ],
        cronExpression: [
          { required: true, trigger: ['blur', 'change'], validator: checkCronExpression },
        ],
        enabled: [
          { required: false, trigger: ['blur', 'change'] },
        ],
        backupPath: [
          { required: true, trigger: ['blur', 'change'], validator: checkBackupPath },
        ],
        repositoriesKeys: [
          { required: true, trigger: ['blur', 'change'], validator: checkRepositoriesKeys},
        ],
      },
      backupStrategyList: [],
      showRepositoriesModal: false,
      backupStrategyRepositories: [],
      backupStrategyQuery: {
        page: 1,
        limit: 10,
        total: 0,
        strategyName: undefined,
        storageId: undefined,
        repositoryId: undefined,
      },
      backupStrategyLoading: false,
      backupStrategyColumns: [
        {
          title: '备份策略名称',
          i18nKey: 'BackupStrategy.StrategyName',
          dataIndex: 'strategyName',
          key: 'strategyName',
          width: 150,
          scopedSlots: { customRender: 'strategyName' },
        },
        {
          title: '仓库信息',
          i18nKey: 'BackupStrategy.Repositories',
          dataIndex: 'repositories',
          key: 'repositories',
          width: 200,
          scopedSlots: { customRender: 'repositories' },
          customCell: () => {
            return {
              style: {
                maxWidth: '220px',
                overflow: 'hidden',
                whiteSpace: 'nowrap',
                textOverflow: 'ellipsis',
                cursor: 'pointer'
              }
            }
          },
        },
        {
          title: 'Cron设置',
          i18nKey: 'BackupStrategy.CronExpression',
          dataIndex: 'cronExpression',
          key: 'cronExpression',
          width: 150,
          scopedSlots: { customRender: 'cronExpression' },
        },
        {
          title: '启用',
          i18nKey: 'BackupStrategy.Enabled',
          dataIndex: 'enabled',
          key: 'enabled',
          width: 50,
          scopedSlots: { customRender: 'enabled' },
        },
        {
          title: '增量备份',
          i18nKey: 'BackupStrategy.Incremental',
          dataIndex: 'incremental',
          key: 'incremental',
          width: 50,
          scopedSlots: { customRender: 'incremental' },
        },
        {
          title: '操作',
          i18nKey: 'BackupStrategy.Operate',
          dataIndex: 'operation',
          width: 220,
          scopedSlots: { customRender: 'operation' },
        },
      ],
      repositoriesList: [],
      selectedRepositoriesKeys: [],
      repositoriesLoading: false,
      repositoriesHasMore: false,
      repositoriesPage: 1,
      repositoriesLimit: 100000,
      repositoriesTotal: 0,
    }
  },
  components: {
  },
  computed: {
    i18nBackupStrategyColumns() {
      return this.backupStrategyColumns.map(column => {
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
        message = this.$t('BackupStrategy.OperationSuccessful');
      }
      this.$notification["success"]({
        message: message,
        description: "",
      });
    },
    initData() {
      this.queryBackupStrategy()
      this.queryRepositories()
      this.folderList()
    },
    folderList() {
      folderList({ directoryPath: "/" }).then(res => {
        this.directoryPaths = []
        if (res) {
          res.forEach(item => {
            this.directoryPaths.push({
              key: item.fullPath,
              title: item.name,
              value: item.fullPath,
              isLeaf: !item.hasSubDirectories
            })
          })
        }
      })
    },
    //异步加载树形数据
    onLoadData(treeNode) {
      return new Promise((resolve) => {
        if (treeNode.dataRef.children) {
          resolve()
          return
        }
        let child = []
        folderList({ directoryPath: treeNode.dataRef.value }).then((res) => {
          if (res) {
            res.forEach((item) => {
              let obj = {}
              obj = {
                key: item.fullPath,
                title: item.name,
                value: item.fullPath,
                isLeaf: !item.hasSubDirectories
              };
              child.push(obj)
            })
            treeNode.dataRef.children = child
            this.directoryPaths = [...this.directoryPaths]
            resolve()
          }
        })
      })
    },
    showBackupStrategyRepositories(item) {
      this.backupStrategyRepositories = []
      if (item.backupStrategyRepositories) {
        this.backupStrategyRepositories = item.backupStrategyRepositories
      }
      this.showRepositoriesModal = true
    },
    queryBackupStrategy() {
      this.backupStrategyLoading = true
      queryBackupStrategy(this.backupStrategyQuery).then(res => {
        this.backupStrategyList = []
        if (res && res.data) {
          this.backupStrategyList = res.data.rows
          this.backupStrategyQuery.total = res.data.total
        }
      }).finally(() => {
        this.backupStrategyLoading = false
      })
    },
    getBackupStrategy(backupStrategy) {
      getBackupStrategy({ strategyName: backupStrategy.strategyName }).then(res => {
        if (res) {
          this.backupStrategyForm.id = backupStrategy.id
          this.backupStrategyForm.strategyName = res.strategyName
          this.backupStrategyForm.cronExpression = res.cronExpression
          this.backupStrategyForm.backupPath = res.backupPath
          this.backupStrategyForm.incremental = res.incremental
          this.backupStrategyForm.retentionPeriod = res.retentionPeriod
          this.backupStrategyForm.enabled = res.enabled
          this.selectedRepositoriesKeys = res.repositories
          this.incrementalChange()
        }
      }).finally(() => {
      })
    },
    handleChangeTable(pagination, filters, sorter) {
      if (pagination) {
        this.backupStrategyQuery.page = pagination.current
      }
      this.queryBackupStrategy()
    },
    handheSearch() {
      this.backupStrategyQuery.page = 1
      this.queryBackupStrategy()
    },
    backupStrategyDelete(item) {
      deleteBackupStrategy({ strategyName: item.strategyName }).then(res => {
        this.successMsg(this.$t('BackupStrategy.OperationSuccessful'))
        this.queryBackupStrategy()
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
    showBackupStrategyInfo(item) {
      this.backupStrategyResetForm()
      if (item) {
        this.getBackupStrategy(item)
      }
      this.backupStrategyVisible = true
    },
    backupStrategyClose() {
      this.backupStrategyResetForm()
      this.backupStrategyVisible = false
    },
    backupStrategyFormSubmit() {
      this.$refs.backupStrategyFormRef.validate(valid => {
        if (valid) {
          let dataForm = Object.assign({}, this.backupStrategyForm)
          dataForm.repositories = this.selectedRepositoriesKeys
          if (dataForm.id) {
            updateBackupStrategy(dataForm).then(res => {
              this.successMsg(this.$t('BackupStrategy.OperationSuccessful'))
              this.queryBackupStrategy()
              this.backupStrategyClose()
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
            saveBackupStrategy(dataForm).then(res => {
              this.successMsg(this.$t('BackupStrategy.OperationSuccessful'))
              this.queryBackupStrategy()
              this.backupStrategyClose()
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
    backupStrategyResetForm() {
      this.backupStrategyForm = {
        id: null,
        strategyName: '',
        cronExpression: '',
        enabled: true,
        backupPath: null,
        incremental: false,
        retentionPeriod: 7,
        repositories: [],
      }
      this.retentionPeriodDisabled = false
      this.selectedRepositoriesKeys = []
      if (this.$refs.backupStrategyFormRef) {
        this.$refs.backupStrategyFormRef.resetFields()
      }
    },
    queryRepositories(isLoadMore) {
      if (!isLoadMore) {
        this.repositoriesPage = 1
        this.repositoriesList = []
        this.repositoriesHasMore = true
      }
      if (!this.repositoriesHasMore || this.repositoriesLoading) {
        return
      }
      this.repositoriesLoading = true;
      queryRepositories({ page: this.repositoriesPage, limit: this.repositoriesLimit, excludeType: 'group'}).then(res => {
        const newRepositories = res.data.rows.map((item) => {
          let temp = {}
          temp.key = `${item.storageId}:${item.id}`
          temp.title = temp.key
          temp.label = temp.key
          return temp
        })
        this.repositoriesList = [...this.repositoriesList, ...newRepositories]
        this.repositoriesTotal = res.data.total
        this.repositoriesHasMore = this.repositoriesList.length < this.repositoriesTotal
        this.repositoriesPage++
        this.selectedRepositoriesKeys.forEach(key => {
          if (!this.repositoriesList.some(use => use.key === key)) {
            let temp = {}
            temp.key = key
            temp.title = temp.key
            temp.label = temp.key
            this.repositoriesList.push(temp)
          }
        });
      }).catch(error => {
        console.error('Error fetching repositories:', error);
      }).finally(() => {
        this.repositoriesLoading = false;
      });
    },
    repositoriesOnChange(nextTargetKeys) {
      this.selectedRepositoriesKeys = nextTargetKeys
    },
    handleRepositoriesScroll(direction, e) {
      if (direction === 'left') {
        this.$nextTick(() => {
          const target = e.target
          const { scrollTop, clientHeight, scrollHeight } = target;
          if (scrollTop + clientHeight >= scrollHeight - 50) {
            if (!this.repositoriesLoading && this.repositoriesHasMore) {
              this.queryRepositories(true)
            }
          }
        })
      }
    },
    execute(record) {
      executeBackup({strategyName: record.strategyName}).then(res => {
        this.successMsg(this.$t('BackupStrategy.OperationSuccessful'))
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
    incrementalChange() {
      if (this.backupStrategyForm.incremental) {
        this.backupStrategyForm.retentionPeriod = 7
        this.retentionPeriodDisabled = true
        return;
      }
      this.retentionPeriodDisabled = false
    }
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
.backup-strategy-add {
  vertical-align: middle;
}
.backup-form-common {
  width: 450px
}
</style>
