<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-descriptions title="最近一次迁移" :column="1" class="mb-20" v-if="record.info">
          <a-descriptions-item label="操作用户">
            {{ record.info.operator }}
          </a-descriptions-item>
          <a-descriptions-item label="操作时间">
            {{ record.createTime }}
          </a-descriptions-item>
          <a-descriptions-item label="存储空间">
            {{ record.info.storageId }}
          </a-descriptions-item>
          <a-descriptions-item label="所属仓库">
            {{ record.info.repositoryId }}
          </a-descriptions-item>
          <a-descriptions-item label="制品总数">
            {{ record.info.artifactsCount }}
          </a-descriptions-item>
          <a-descriptions-item label="迁移制品">
            {{ record.info.process }}
          </a-descriptions-item>
          <a-descriptions-item label="迁移进度">
            {{ record.info.progress + '%'}}
          </a-descriptions-item>
          <a-descriptions-item label="迁移状态">
            <a-tag v-if="record.comment"
              :color="record.comment.indexOf('完成') !== -1 ? 'green' : record.comment.indexOf('错误') !== -1 ? 'red' : 'orange'">
              {{ record.comment }}
              <a-popconfirm title="确定要更改状态吗？" okType="danger" ok-text="确定" cancel-text="取消"
                @confirm="updateSingleDict(record.id, '手动结束')">
                <a-icon type="unlock" theme="filled" v-if="record.comment.indexOf('中') !== -1" />
              </a-popconfirm>
            </a-tag>
            <span v-else>--</span>
          </a-descriptions-item>
        </a-descriptions>
        <a-form-model layout="horizontal" ref="dataMigrationForm" :model="dataMigrationForm" :rules="dataMigrationRules" :hideRequiredMark="true">
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
                      @confirm="dataMigrationFormSubmit" :disabled="record.comment && record.comment.length > 0 && record.comment.indexOf('中') > -1">
                      <a-button type="danger" :disabled="record.comment && record.comment.length > 0 && record.comment.indexOf('中') > -1">
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
  getSingleDict,
  updateSingleDict,
  syncArtifactProvider
} from "@/api/advanced"

export default {
  inject: ["reload"],
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
      dataMigrationRules:  {
        storageId: [{ required: true, message: "请选择存储空间", trigger: "blur" }],
        repositoryId: [{ required: true, message: "请选择所属仓库", trigger: "blur" }],
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
        message = "操作成功"
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
        this.message("success", "状态更新成功")
      })
    }
  }
}
</script>

<style lang="scss" scoped></style>