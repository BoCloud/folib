<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-descriptions :title="$t('Setting.LastMigration')" :column="1" class="mb-20" v-if="record.info">
          <a-descriptions-item :label="$t('Setting.OperationUser')">
            {{ record.info.operator }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Setting.OperationTime')">
            {{ record.createTime }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Setting.StorageSpace')">
            {{ record.info.storageId }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Setting.OwnedWarehouse')">
            {{ record.info.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Setting.ProductsNum')">
            {{ record.info.artifactsCount }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Setting.MigratedProducts')">
            {{ record.info.process }}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Setting.MigrationProgress')">
            {{ record.info.progress + '%'}}
          </a-descriptions-item>
          <a-descriptions-item :label="$t('Setting.TransitionState')">
            <a-tag v-if="record.comment"
              :color="record.comment.indexOf('完成') !== -1 ? 'green' : record.comment.indexOf('错误') !== -1 ? 'red' : 'orange'">
              {{ record.comment }}
              <a-popconfirm :title="$t('Setting.sureChangeState')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                @confirm="updateSingleDict(record.id, $t('Setting.ManualClosing'))">
                <a-icon type="unlock" theme="filled" v-if="record.comment.indexOf('中)') !== -1" />
              </a-popconfirm>
            </a-tag>
            <span v-else>--</span>
          </a-descriptions-item>
        </a-descriptions>
        <a-form-model layout="horizontal" ref="dataMigrationForm" :model="dataMigrationForm" :rules="dataMigrationRules" :hideRequiredMark="true">
          <a-row :gutter="[24]">
            <a-col :span="24">
              <a-row :gutter="[24]">
                <a-col :span="6">
                  <a-form-model-item class="mb-10" :label="$t('Setting.StorageSpace')" :colon="false" prop="storageId">
                    <a-select @change="handleStorageChange" showSearch allowClear v-model="dataMigrationForm.storageId"
                      :placeholder="$t('Setting.selectTheStorageSpace')">
                      <a-select-option v-for="storageId in storages" :key="storageId">
                        {{ storageId }}
                      </a-select-option>
                    </a-select>
                  </a-form-model-item>
                </a-col>
                <a-col :span="6">
                  <a-form-model-item class="mb-10" :label="$t('Setting.OwnedWarehouse')" :colon="false" prop="repositoryId">
                    <a-select showSearch @change="handleRepositoryChange" allowClear
                      v-model="dataMigrationForm.repositoryId" :placeholder="$t('Setting.selectYourRepository')">
                      <a-select-option v-for="repositoryId in repositories" :key="repositoryId">
                        {{ repositoryId }}
                      </a-select-option>
                    </a-select>
                  </a-form-model-item>
                </a-col>
                <a-col :span="4">
                  <a-form-model-item class="mb-10" :label="$t('Setting.BatchSize')" :colon="false" prop="batch">
                    <a-input :placeholder="$t('Setting.enterTheBatchNumber')" v-model="dataMigrationForm.batch" />
                  </a-form-model-item>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-form-model-item :wrapper-col="{ span: 14, offset: 6 }">
                    <a-popconfirm :title="$t('Setting.surePerformDataMigration')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                      @confirm="dataMigrationFormSubmit" :disabled="record.comment && record.comment.length > 0 && record.comment.indexOf('中') > -1">
                      <a-button type="danger" :disabled="record.comment && record.comment.length > 0 && record.comment.indexOf('中') > -1">
                        {{ $t('Setting.Save') }}
                      </a-button>
                    </a-popconfirm>
                    <a-button class="ml-10" @click="dataMigrationResetForm">
                      {{ $t('Setting.Cancel') }}
                    </a-button>
                  </a-form-model-item>
                </a-col>
              </a-row>
            </a-col>
          </a-row>
        </a-form-model>
      </a-col>
    </a-row>
  </div>
</template>
<script>
import {
  getStoragesAndRepositories
} from "@/api/folib"
import {
  getSingleDict,
  updateSingleDict,
  syncArtifactProvider
} from "@/api/advanced"

export default {
  inject: ["reload"],
  data() {
    const checkStorageId = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Setting.selectTheStorageSpace')))
      } else {
        callback()
      }
    }
    const checkRepositoryId = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Setting.selectYourRepository')))
      } else {
        callback()
      }
    }
    return {
      storages: [],
      repositoriesData: {},
      repositories: [],
      dataMigrationForm: {
        storageId: undefined,
        repositoryId: undefined,
        type: "layout",
        batch: 500,
      },
      dataMigrationRules:  {
        storageId: [{ required: true, trigger: "blur", validator: checkStorageId }],
        repositoryId: [{ required: true, trigger: "blur", validator: checkRepositoryId }],
      },
      record: {
        id: 0,
        createTime: "",
        comment: "",
        info: {
          fail: 0,
          process: 0,
          artifactsCount: 0,
          success: 0,
          takeTime: 0,
          repositoryId: "",
          progress: 0.00,
          mavenIndexerFileName: "",
          lines: 0,
          operator: "",
          storageId: ""
        }
      }
    }
  },
  components: {
  },
  computed: {

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
      this.getSingleDict("handler_maven_indexer")
    },
    queryStoragesAndRepositories() {
      getStoragesAndRepositories({ excludeType: 'group', layout: 'Maven 2', type: 'proxy' }).then(res => {
        if (res) {
          this.storages = []
          this.repositoriesData = {}
          res.forEach(item => {
            if (item.children && item.children.length > 0) {
              this.storages.push(item.id)
              this.repositoriesData[item.id] = []
              item.children.forEach(children => {
                this.repositoriesData[item.id].push(children.id)
              })
            }
          })
        }
      })
    },
    handleStorageChange(value) {
      this.repositories = this.repositoriesData[value]
    },
    handleRepositoryChange() {
    },
    dataMigrationFormSubmit() {
      this.$refs.dataMigrationForm.validate(valid => {
        if (valid) {
          syncArtifactProvider(this.dataMigrationForm).then(res => {
            if (res) {
              setTimeout(() => {
                this.getSingleDict("handler_maven_indexer")
              }, 100)
              this.message("success", this.$t('Setting.dataMigrationTaskStart'))
            }
          }).catch((err) => {
            this.message("error", this.$t('Setting.FailPerformMigration'))
          }).finally(() => {
            this.dataMigrationResetForm()
          })
        }
      })
    },
    dataMigrationResetForm() {
      this.$refs.dataMigrationForm.resetFields()
      this.initData()
    },
    getSingleDict(dictType) {
      this.record = {

      }
      getSingleDict({ dictType: dictType }).then(res => {
        if (res) {
          this.record = res
          if (res.dictValue) {
            this.record.info = JSON.parse(res.dictValue)
          }
        }
      })
    },
    updateSingleDict(id, comment) {
      updateSingleDict({ id: id, comment: comment }).then(res => {
        this.initData()
        this.message("success", this.$t('Setting.updateStatusSuccessful'))
      })
    }
  }
}
</script>

<style lang="scss" scoped></style>
