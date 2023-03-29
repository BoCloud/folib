<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-tabs class="tabs-sliding" default-active-key="1">
          <a-tab-pane key="1" tab="构建数据">
            <a-card :bordered="false" class="header-solid">
              <template #title>
                <h6>构建制品索引</h6>
                <p>该功能用于构建制品索引数据，请谨慎使用</p>
              </template>
              <a-form-model layout="horizontal" ref="buildGraphIndexForm" :model="buildGraphIndexForm"
                :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-row :gutter="[24]">
                      <a-col :span="4">
                        <a-form-model-item class="mb-10" label="存储空间" :colon="false" prop="storageId">
                          <a-select @change="handleStorageChange" showSearch allowClear v-model="buildGraphIndexForm.storageId"
                            placeholder="请选择存储空间">
                            <a-select-option v-for="storageId in storages" :key="storageId">
                              {{ storageId }}
                            </a-select-option>
                          </a-select>
                        </a-form-model-item>
                      </a-col>
                      <a-col :span="4">
                        <a-form-model-item class="mb-10" label="所属仓库" :colon="false" prop="repositoryId">
                          <a-select showSearch @change="handleRepositoryChange" allowClear v-model="buildGraphIndexForm.repositoryId"
                            placeholder="请选择所属仓库">
                            <a-select-option v-for="repositoryId in repositories" :key="repositoryId">
                              {{ repositoryId }}
                            </a-select-option>
                          </a-select>
                        </a-form-model-item>
                      </a-col>
                    </a-row>
                    <a-row :gutter="[24]">
                      <a-col :span="8">
                        <a-form-model-item class="mb-10" :colon="false" prop="path">
                          <template slot="label">
                            制品绝对路径
                            <a-popover placement="topLeft">
                              <template slot="content">
                                <p class="mb-0">可指定目录进行构建数据，若不填写则为仓库的根目录</p>
                                <p class="mb-0">目录为绝对路径（容器部署，则为容器内部的绝对路径）</p>
                              </template>
                              <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                            </a-popover>
                          </template>
                          <a-input v-model="buildGraphIndexForm.path" placeholder="请输入制品绝对路径" />
                        </a-form-model-item>
                      </a-col>
                    </a-row>
                    <a-row :gutter="[24]">
                      <a-col :span="12">
                        <a-form-model-item :wrapper-col="{ span: 14, offset: 6 }">
                          <a-popconfirm title="确定要构建数据吗？" okType="danger" ok-text="确定" cancel-text="取消"
                            @confirm="buildGraphIndexFormSubmit">
                            <a-button type="danger">
                              保存
                            </a-button>
                          </a-popconfirm>
                          <a-button class="ml-10" @click="buildGraphIndexResetForm">
                            取消
                          </a-button>
                        </a-form-model-item>
                      </a-col>
                    </a-row>
                  </a-col>
                </a-row>
              </a-form-model>
            </a-card>
          </a-tab-pane>
          <a-tab-pane key="2" tab="漏洞更新">
            <a-card :bordered="false" class="header-solid">
              <template #title>
                <h6>更新漏洞数据</h6>
                <p>该功能用于更新漏洞数据至本地漏洞库</p>
              </template>
              <a-form-model layout="horizontal" ref="vulnerabilitiesForm" :model="vulnerabilitiesForm"
                :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-row :gutter="[24]">
                      <a-col :span="12">
                        <a-form-model-item>
                          <a-popconfirm title="确定要更新漏洞数据吗？" okType="danger" ok-text="确定" cancel-text="取消"
                            @confirm="vulnerabilitiesFormSubmit">
                            <a-button type="danger">
                              更新
                            </a-button>
                          </a-popconfirm>
                        </a-form-model-item>
                      </a-col>
                    </a-row>
                  </a-col>
                </a-row>
              </a-form-model>
            </a-card>
          </a-tab-pane>
        </a-tabs>
      </a-col>
    </a-row>
  </div>
</template>
<script>
import {
  getStoragesAndRepositories
} from "@/api/folib"
import {
  buildGraphIndex
} from "@/api/artifact"
import {
  vulnerabilitiesDataUpdate
} from "@/api/settings"

export default {
  data() {
    return {
      storages: [],
      repositoriesData: {},
      repositories: [],
      buildGraphIndexForm: {
        storageId: undefined,
        repositoryId: undefined,
        path: ''
      },
      vulnerabilitiesForm: {

      },
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
      getStoragesAndRepositories({ excludeType: 'group' }).then(res => {
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
      this.buildGraphIndexForm.repositoryId = undefined
      this.buildGraphIndexForm.path = ''
      this.repositories = this.repositoriesData[value]
    },
    handleRepositoryChange() {
      this.buildGraphIndexForm.path = ''
    },
    buildGraphIndexFormSubmit() {
      this.$refs.buildGraphIndexForm.validate(valid => {
        if (valid) {
          buildGraphIndex(this.buildGraphIndexForm).then(res => {
            if (res) {
              this.message("warning", "有构建数据任务正在执行，请稍后重试")
            } else {
              this.message("success", "请稍等，构建数据任务已启动，正在异步执行")
            }
          }).catch((err) => {
            this.message("error", "执行构建数据失败")
          }).finally(() => {

          })
        }
      })
    },
    buildGraphIndexResetForm() {
      this.$refs.buildGraphIndexForm.resetFields()
      this.initData()
    },
    vulnerabilitiesFormSubmit() {
      this.$refs.vulnerabilitiesForm.validate(valid => {
        if (valid) {
          vulnerabilitiesDataUpdate().then(res => {
            this.message("success", "请稍等，漏洞数据更新任务已启动，正在异步执行")
          }).catch((err) => {
            this.message("error", "执行漏洞更新失败")
          }).finally(() => {

          })
        }
      })
    }
  },
}
</script>

<style lang="scss" scoped></style>