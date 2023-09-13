<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-form-model layout="horizontal" ref="dataMigrationForm" :model="dataMigrationForm" :hideRequiredMark="true">
          <a-row :gutter="[24]">
            <a-col :span="24">
              <a-row :gutter="[24]">
                <a-col :span="4">
                  <a-form-model-item class="mb-10" label="存储空间" :colon="false" prop="storageId">
                    <a-select @change="handleStorageChange" showSearch allowClear v-model="dataMigrationForm.storageId"
                      placeholder="请选择存储空间">
                      <a-select-option v-for="storageId in storages" :key="storageId">
                        {{ storageId }}
                      </a-select-option>
                    </a-select>
                  </a-form-model-item>
                </a-col>
                <a-col :span="4">
                  <a-form-model-item class="mb-10" label="所属仓库" :colon="false" prop="repositoryId">
                    <a-select showSearch @change="handleRepositoryChange" allowClear
                      v-model="dataMigrationForm.repositoryId" placeholder="请选择所属仓库">
                      <a-select-option v-for="repositoryId in repositories" :key="repositoryId">
                        {{ repositoryId }}
                      </a-select-option>
                    </a-select>
                  </a-form-model-item>
                </a-col>
                <a-col :span="4">
                  <a-form-model-item class="mb-10" label="批处理数量" :colon="false" prop="batch">
                    <a-input placeholder="请输入批处理数量" v-model="dataMigrationForm.batch" />
                  </a-form-model-item>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-form-model-item :wrapper-col="{ span: 14, offset: 6 }">
                    <a-popconfirm title="确定要执行数据迁移吗？" okType="danger" ok-text="确定" cancel-text="取消"
                      @confirm="dataMigrationFormSubmit">
                      <a-button type="danger">
                        保存
                      </a-button>
                    </a-popconfirm>
                    <a-button class="ml-10" @click="dataMigrationResetForm">
                      取消
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
  syncArtifactProvider
} from "@/api/advanced"

export default {
  data() {
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
      process: {}
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
        message = "操作成功"
      }
      this.$notification[type]({
        message: message,
        description: "",
      })
    },
    initData() {
      this.queryStoragesAndRepositories()
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
                // this.getSingleDict()
              }, 100)
              this.message("success", "请稍等，数据迁移任务已启动，正在异步执行")
            }
          }).catch((err) => {
            this.message("error", "执行数据迁移任务失败")
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
      this.process = {
       
      }
      getSingleDict({ dictType: dictType }).then(res => {
        if (res) {
          this.process = res
        }
      })
    },
  }
}
</script>

<style lang="scss" scoped></style>