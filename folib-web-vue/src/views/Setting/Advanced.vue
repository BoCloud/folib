<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-tabs class="tabs-sliding" :default-active-key="1" @change="tabChange($event)">
          <a-tab-pane :key="1" tab="构建数据">
            <a-card :bordered="false" class="header-solid">
              <template #title>
                <h6>构建制品索引</h6>
                <p>该功能用于构建制品索引数据，请谨慎使用
                </p>
              </template>
              <a-descriptions title="最近一次构建" :column="1" class="mb-20">
                <a-descriptions-item label="用户">
                  {{ singleDict.dictKey }}
                </a-descriptions-item>
                <a-descriptions-item label="时间">
                  {{ singleDict.createTime }}
                </a-descriptions-item>
                <a-descriptions-item label="参数">
                  {{ singleDict.dictValue }}
                </a-descriptions-item>
                <a-descriptions-item label="状态">
                  <a-tag v-if="singleDict.comment"
                    :color="singleDict.comment.indexOf('完成') !== -1 ? 'green' : singleDict.comment.indexOf('错误') !== -1 ? 'red' : 'orange'">
                    {{ singleDict.comment }}
                    <a-popconfirm title="确定要更改状态吗？" okType="danger" ok-text="确定" cancel-text="取消"
                      @confirm="updateSingleDict(1, singleDict.id, '手动结束')">
                      <a-icon type="unlock" theme="filled" v-if="singleDict.comment.indexOf('中') !== -1" />
                    </a-popconfirm>
                  </a-tag>
                  <span v-else>--</span>
                </a-descriptions-item>
              </a-descriptions>
              <a-form-model layout="horizontal" ref="buildGraphIndexForm" :model="buildGraphIndexForm"
                :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-row :gutter="[24]">
                      <a-col :span="4">
                        <a-form-model-item class="mb-10" label="存储空间" :colon="false" prop="storageId">
                          <a-select @change="handleStorageChange" showSearch allowClear
                            v-model="buildGraphIndexForm.storageId" placeholder="请选择存储空间">
                            <a-select-option v-for="storageId in storages" :key="storageId">
                              {{ storageId }}
                            </a-select-option>
                          </a-select>
                        </a-form-model-item>
                      </a-col>
                      <a-col :span="4">
                        <a-form-model-item class="mb-10" label="所属仓库" :colon="false" prop="repositoryId">
                          <a-select showSearch @change="handleRepositoryChange" allowClear
                            v-model="buildGraphIndexForm.repositoryId" placeholder="请选择所属仓库">
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
                            @confirm="buildGraphIndexFormSubmit" :disabled="singleDict.comment.indexOf('中') > -1">
                            <a-button type="danger" :disabled="singleDict.comment.indexOf('中') > -1">
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
          <a-tab-pane :key="2" tab="漏洞更新">
            <a-card :bordered="false" class="header-solid">
              <template #title>
                <h6>更新漏洞数据</h6>
                <p>该功能用于更新漏洞数据至本地漏洞库
                </p>
              </template>
              <a-descriptions title="最近一次更新" :column="1" class="mb-20">
                <a-descriptions-item label="用户">
                  {{ singleDict.dictKey }}
                </a-descriptions-item>
                <a-descriptions-item label="时间">
                  {{ singleDict.createTime }}
                </a-descriptions-item>
                <a-descriptions-item label="状态">
                  <a-tag v-if="singleDict.comment"
                    :color="singleDict.comment.indexOf('完成') !== -1 ? 'green' : singleDict.comment.indexOf('错误') !== -1 ? 'red' : 'orange'">
                    {{ singleDict.comment }}
                    <a-popconfirm title="确定要更改状态吗？" okType="danger" ok-text="确定" cancel-text="取消"
                      @confirm="updateSingleDict(2, singleDict.id, '手动结束')">
                      <a-icon type="unlock" theme="filled" v-if="singleDict.comment.indexOf('中') !== -1" />
                    </a-popconfirm>
                  </a-tag>
                  <span v-else>--</span>
                </a-descriptions-item>
              </a-descriptions>
              <a-form-model layout="horizontal" ref="vulnerabilitiesForm" :model="vulnerabilitiesForm"
                :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-row :gutter="[24]">
                      <a-col :span="12">
                        <a-form-model-item>
                          <a-popconfirm title="确定要更新漏洞数据吗？" okType="danger" ok-text="确定" cancel-text="取消"
                            @confirm="vulnerabilitiesFormSubmit" :disabled="singleDict.comment.indexOf('中') > -1">
                            <a-button type="danger" :disabled="singleDict.comment.indexOf('中') > -1">
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
          <a-tab-pane :key="3" tab="DB信息">
            <a-tabs class="tabs-sliding" :default-active-key="1" @change="dbTabChange($event)">
              <a-tab-pane :key="1" tab="Schema">
                <a-card :bordered="false" class="header-solid">
                  <template #title>
                    <h6>Schema信息</h6>
                    <p>该功能用于查看Schema信息</p>
                  </template>
                  <a-row :gutter="[24]">
                    <a-col :span="24">
                      <prism-editor class="metadata-prism-editor" v-model="janusGraphInfo.schema"
                        :highlight="highlighterHandle" :line-numbers="true" :readonly="true">
                      </prism-editor>
                    </a-col>
                  </a-row>
                </a-card>
              </a-tab-pane>
              <a-tab-pane :key="2" tab="索引操作">
                <a-card :bordered="false" class="header-solid">
                  <template #title>
                    <h6>索引操作</h6>
                    <p>该功能用于索引的重建、注册</p>
                  </template>
                  <a-row :gutter="[24]">
                    <a-col :span="24">
                      <a-form-model layout="horizontal" ref="janusGraphIndexForm" :model="janusGraphIndexForm"
                        :rules="janusGraphIndexRules" :hideRequiredMark="false">
                        <a-row :gutter="[24]">
                          <a-col :span="24">
                            <a-row :gutter="[24]">
                              <a-col :span="6">
                                <a-form-model-item label="索引名称" :colon="false" prop="indexName">
                                  <a-input placeholder="请输入索引名称" v-model="janusGraphIndexForm.indexName" />
                                </a-form-model-item>
                              </a-col>
                            </a-row>
                            <a-row :gutter="[24]">
                              <a-col :span="12">
                                <a-form-model-item>
                                  <a-popconfirm title="确定要重建该索引吗？" okType="danger" ok-text="确定" cancel-text="取消"
                                    @confirm="janusGraphIndexFormSubmit(1)">
                                    <a-button type="danger" class="mr-20">
                                      重建索引
                                    </a-button>
                                  </a-popconfirm>
                                  <a-popconfirm title="确定要注册该索引吗？" okType="danger" ok-text="确定" cancel-text="取消"
                                    @confirm="janusGraphIndexFormSubmit(2)">
                                    <a-button type="danger">
                                      注册索引
                                    </a-button>
                                  </a-popconfirm>
                                </a-form-model-item>
                              </a-col>
                            </a-row>
                          </a-col>
                        </a-row>
                      </a-form-model>
                    </a-col>
                  </a-row>
                </a-card>
              </a-tab-pane>
              <a-tab-pane :key="3" tab="实例操作">
                <a-card :bordered="false" class="header-solid">
                  <template #title>
                    <h6>实例操作</h6>
                    <p>该功能用于查看、删除实例操作，下方为实例列表</p>
                  </template>
                  <a-row :gutter="[24]">
                    <a-col :span="6" v-for="(instance, index) in janusGraphInfo.openInstances" :key="index">
                      <a-tag :color="instance.indexOf('(current)') !== -1 ? 'red' : 'green'">
                        {{ instance }}
                        <a-popconfirm title="确定要删除该实例吗？" okType="danger" ok-text="确定" cancel-text="取消"
                          @confirm="removeInstance(instance)">
                          <a-icon type="close" v-if="instance.indexOf('(current)') === -1" />
                        </a-popconfirm>
                      </a-tag>
                    </a-col>
                  </a-row>
                </a-card>
              </a-tab-pane>
            </a-tabs>
          </a-tab-pane>
        </a-tabs>
      </a-col>
    </a-row>
  </div>
</template>
<script>
import { PrismEditor } from "vue-prism-editor"
import "vue-prism-editor/dist/prismeditor.min.css"
import { highlight, languages } from "prismjs/components/prism-core"
import "prismjs/components/prism-clike"
import "prismjs/components/prism-javascript"
import "prismjs/themes/prism-tomorrow.css"
import {
  getStoragesAndRepositories
} from "@/api/folib"
import {
  buildGraphIndex
} from "@/api/artifact"
import {
  vulnerabilitiesDataUpdate
} from "@/api/settings"
import {
  janusGraph,
  deleteInstance,
  reindex,
  registerIndex,
  getSingleDict,
  updateSingleDict,
} from "@/api/advanced"

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
      janusGraphInfo: {
        openInstances: [],
        schema: '',
      },
      janusGraphIndexForm: {
        indexName: "",
      },
      janusGraphIndexRules: {
        indexName: [
          { required: true, message: '请输入索引名称', trigger: 'blur' },
        ],
      },
      singleDict: {
        createTime: '--',
        dictKey: '--',
        dictValue: '--',
        comment: ''
      },
    }
  },
  components: {
    PrismEditor
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
    highlighterHandle(code) {
      return highlight(code, languages.js)
    },
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
      this.getSingleDict('build_graph_index')
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
              setTimeout(() => {
                this.getSingleDict('build_graph_index')
              }, 100)
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
            setTimeout(() => {
              this.getSingleDict('vulnerability_data_update')
            }, 100)
            this.message("success", "请稍等，漏洞数据更新任务已启动，正在异步执行")
          }).catch((err) => {
            this.message("error", "执行漏洞更新失败")
          }).finally(() => {

          })
        }
      })
    },
    getJanusGraphInfo() {
      janusGraph().then(res => {
        if (res) {
          this.janusGraphInfo = res
        }
      }).catch((err) => {
      }).finally(() => {
      })
    },
    removeInstance(instance) {
      if (instance) {
        if (instance.indexOf('(current)') !== -1) {
          this.message("warning", "当前实例不允许删除")
          return false
        }
        deleteInstance(instance).then(res => {
          this.message("success", "删除实例成功")
          this.getJanusGraphInfo()
        }).catch((err) => {
        }).finally(() => {
        })
      }
    },
    janusGraphIndexFormSubmit(type) {
      this.$refs.janusGraphIndexForm.validate(valid => {
        if (valid) {
          let data = {
            indexNames: [this.janusGraphIndexForm.indexName]
          }
          if (type === 1) {
            reindex(data).then(res => {
              this.message("success", "重建索引执行成功")
            }).catch((err) => {
              let msg = err.response.data.error ? err.response.data.error : '执行重建索引失败'
              this.message("error", msg)
            }).finally(() => {
            })
          } else if (type === 2) {
            registerIndex(data).then(res => {
              this.message("success", "注册索引执行成功")
            }).catch((err) => {
              let msg = err.response.data.error ? err.response.data.error : '执行注册索引失败'
              this.message("error", msg)
            }).finally(() => {
            })
          }
        }
      })
    },
    tabChange(activeTab) {
      if (activeTab === 1) {
        this.buildGraphIndexResetForm()
      } else if (activeTab === 2) {
        this.getSingleDict('vulnerability_data_update')
      } else if (activeTab === 3) {
        this.getJanusGraphInfo()
      }
    },
    dbTabChange(activeTab) {
      if (activeTab === 1 || activeTab === 3) {
        this.getJanusGraphInfo()
      } else if (activeTab === 2) {
        if (this.$refs.janusGraphIndexForm) {
          this.$refs.janusGraphIndexForm.resetFields()
        }
      }
    },
    getSingleDict(dictType) {
      this.singleDict = {
        createTime: '--',
        dictKey: '--',
        dictValue: '--',
        comment: ''
      }
      getSingleDict({ dictType: dictType }).then(res => {
        if (res) {
          this.singleDict = res
        }
      })
    },
    updateSingleDict(type, id, comment) {
      updateSingleDict({ id: id, comment: comment }).then(res => {
        this.tabChange(type)
        this.message("success", "状态更新成功")
      })
    }
  }
}
</script>

<style lang="scss" scoped></style>