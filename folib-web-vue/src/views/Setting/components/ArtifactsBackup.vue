<template>
  <div class="artifacts-backup">
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-form :form="backupForm" ref="backupForm" key="backupKey" layout="horizontal" :hideRequiredMark="true">
          <a-row :gutter="[24]">
            <a-col :span="10">
              <a-row :gutter="[24]">
                <a-col :span="24">
                  <a-form-item :label="$t('Setting.BackupRepository')">
                    <gb-ant-select-two-cascader v-decorator="[
                      'repositoryIds',
                      {
                        initialValue: undefined,
                        rules: [{ required: true, message: $t('Setting.selectTheBackupRepository'), type: 'array', }]
                      }
                    ]" allowClear style="width:80%;" :maxTagCount="4" :maxTagTextLength="12" :placeholder="$t('Setting.selectTheBackupRepository')"
                      :selectOptionsConfig="{
                        key: 'key',
                        value: 'key',
                        text: 'id',
                        children: 'children'
                      }" dropdownClassName="customer-multiple-cascader" :treeData="repositories" />
                  </a-form-item>
                </a-col>
                <a-col :span="24">
                  <a-form-item class="mb-10" :label="$t('Setting.BackupDirectory')" :colon="false" prop="repositoryId">
                    <a-tree-select v-if="directoryPathShow" v-decorator="[
                      'directoryPath',
                      {
                        initialValue: undefined,
                        rules: [{ required: true, message: $t('Setting.selectTheBackupDirectory'), }]
                      }
                    ]" style="width:80%" :dropdown-style="{ maxHeight: '400px', overflow: 'auto' }"
                      :tree-data="directoryPaths" :placeholder="$t('Setting.selectTheBackupDirectory')" :load-data="onLoadData" />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-form-item :wrapper-col="{ span: 16, offset: 6 }">
                    <a-popconfirm :title="$t('Setting.sureEnableBackup')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                      @confirm="backupFormSubmit">
                      <a-button type="danger">
                        {{ $t('Setting.Save') }}
                      </a-button>
                    </a-popconfirm>
                    <a-button class="ml-10" @click="backupFormReset">
                      {{ $t('Setting.Cancel') }}
                    </a-button>
                  </a-form-item>
                </a-col>
              </a-row>
            </a-col>

            <a-col :span="14">
              <a-row :gutter="[24]">
                <a-card class="header-solid mr-10">
                  <div class="mx-25 search">
                    <a-col :span="24" class="text-right">
                      <a-input-search :placeholder="$t('Setting.EnterStorageSpace')" class="v-search" v-model="artifactsBackupQuery.storageId"
                        @search="getArtifactsBackupList()" />
                      <a-input-search :placeholder="$t('Setting.EnterTheRepositoryQuery')" class="v-search" v-model="artifactsBackupQuery.repositoryId"
                        @search="getArtifactsBackupList()" />
                    </a-col>
                  </div>
                  <a-table :columns="i18nArtifactsBackupColumns" :data-source="backupInfo.repositories" :scroll="{ x: true }"
                    :loading="loading" :row-key="(r, i) => i.toString()">
                    <div slot="operation" slot-scope="text, record">
                      <div class="col-action">
                        <a-popconfirm :title="$t('Setting.SureDelete')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                          @confirm="artifactsBackupHandlerDelete(record)">
                          <a-button type="link" size="small">
                            <svg width="16" height="16" viewBox="0 0 20 20" fill="none"
                              xmlns="http://www.w3.org/2000/svg">
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
              </a-row>
            </a-col>
          </a-row>
        </a-form>
      </a-col>
    </a-row>
  </div>
</template>
<script>
import {
  getStoragesAndRepositories
} from "@/api/folib"
import {
  folderList,
  saveBackup,
  getDictList,
  deleteDict,
} from "@/api/advanced"

export default {
  inject: ["reload"],
  components: {
  },
  data() {
    return {
      repositories: [],
      directoryPaths: [],
      backupForm: this.$form.createForm(this, { name: 'backupForm' }),
      backupInfo: {
        repositories: [],
        directoryPath: "",
      },
      artifactsBackupColumns: [
        {
          title: '存储空间',
          i18nKey: 'Setting.StorageSpace',
          dataIndex: 'storageId',
          key: 'storageId',
          width: 200,
          scopedSlots: { customRender: 'storageId' },
        },
        {
          title: '所属仓库',
          i18nKey: 'Setting.OwnedWarehouse',
          dataIndex: 'repositoryId',
          key: 'repositoryId',
          width: 200,
          scopedSlots: { customRender: 'repositoryId' },
        },
        {
          title: '备份目录',
          i18nKey: 'Setting.BackupDirectory',
          dataIndex: 'directoryPath',
          key: 'directoryPath',
          width: 200,
          scopedSlots: { customRender: 'directoryPath' },
        },
        {
          title: '操作',
          i18nKey: 'Setting.Operation',
          dataIndex: 'operation',
          width: 100,
          scopedSlots: { customRender: 'operation' },
        },
      ],
      loading: false,
      artifactsBackupQuery: {
        page: 1,
        limit: 10,
        total: 0,
        storageId: undefined,
        repositoryId: undefined,
      },
      directoryPathShow: true,
    }
  },
  computed: {
    i18nArtifactsBackupColumns() {
      return this.artifactsBackupColumns.map(column => {
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

  },
  mounted() { },
  methods: {
    message(type, message) {
      if (!message) {
        message = this.$t('Setting.OperationSuccessful')
      }
      this.$notification[type]({
        message: message,
        description: "",
      })
    },
    initData() {
      this.queryStoragesAndRepositories()
      this.folderList()
      this.getDictList("backup_settings")
    },
    getDictList(dictType) {
      this.backupInfo = {
        repositories: [],
        directoryPath: "",
      }
      getDictList({ dictType: dictType }).then(res => {
        if (res) {
          res.forEach(item => {
            let arr = item.dictKey.split(":")
            this.backupInfo.repositories.push({
              id: item.id,
              key: item.dictKey,
              storageId: arr[0],
              repositoryId: arr[1],
              directoryPath: item.dictValue
            })
          })
        }
      })
    },
    queryStoragesAndRepositories() {
      getStoragesAndRepositories({ excludeType: 'group' }).then(res => {
        if (res) {
          this.repositories = []
          res.forEach(item => {
            if (item.children && item.children.length > 0) {
              this.repositories.push(item)
            }
          })
        }
      })
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
    getArtifactsBackupList() {
      this.loading = true
      this.backupInfo = {
        repositories: [],
        directoryPath: "",
      }
      getDictList({ dictType: "backup_settings" }).then(res => {
        if (res) {
          res.forEach(item => {
            let arr = item.dictKey.split(":")
            this.backupInfo.repositories.push({
              id: item.id,
              key: item.dictKey,
              storageId: arr[0],
              repositoryId: arr[1],
              directoryPath: item.dictValue
            })
          })
          if (this.artifactsBackupQuery.storageId) {
            this.backupInfo.repositories = this.backupInfo.repositories.filter(item => item.storageId.indexOf(this.artifactsBackupQuery.storageId) !== -1)
          }
          if (this.artifactsBackupQuery.repositoryId) {
            this.backupInfo.repositories = this.backupInfo.repositories.filter(item => item.repositoryId.indexOf(this.artifactsBackupQuery.repositoryId) !== -1)
          }
          this.artifactsBackupQuery.total = this.backupInfo.repositories.length
        }
      }).finally(() => {
        this.loading = false
      })
    },
    artifactsBackupHandlerDelete(record) {
      deleteDict({ id : record.id }).then(res => {
        this.message("success", this.$t('Setting.DeleteSuccess'))
      }).catch(err => {
        this.$notification['error']({
          message: err.response.data.error,
          description: ''
        })
      }).finally(() => {
        this.getArtifactsBackupList()
      })
    },
    backupFormSubmit() {
      this.backupForm.validateFields((err, values) => {
        if (!err) {
          let data = {
            repositoryList: [],
            directoryPath: values.directoryPath
          }
          values.repositoryIds.forEach(item => {
            let r = item.split(",")
            data.repositoryList.push({
              storageId: r[0],
              repositoryId: r[1]
            })
          })
          saveBackup(data).then(res => {
            this.directoryPathShow = false
            this.message("success", this.$t('Setting.backupPolicyIsSetSuccessful'))
            setTimeout(() => {
              this.directoryPathShow = true
            }, 50)
          }).catch((err) => {
            this.message("error", this.$t('Setting.FailedToSetBackupPolicy'))
          }).finally(() => {
            this.backupFormReset()
          })
        }
      })
    },
    backupFormReset() {
      this.backupForm.resetFields()
      this.directoryPathShow = true
      this.initData()
    },
  }
}
</script>

<style lang="scss" scoped>
$md: 768px;

.artifacts-backup::v-deep {
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
