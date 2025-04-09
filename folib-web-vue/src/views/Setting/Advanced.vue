<template>
  <div>
    <a-row type="flex" :gutter="24">
      <a-col :span="24" class="mb-24">
        <a-tabs class="tabs-sliding" :default-active-key="1" @change="tabChange($event)">
          <a-tab-pane :key="1" :tab="$t('Setting.BuildData')">
            <a-card :bordered="false" class="header-solid">
              <template #title>
                <h6>{{ $t('Setting.BuildAnArtifactIndex') }}</h6>
                <p>{{ $t('Setting.migrateArtifactData') }}</p>
              </template>
              <a-descriptions :title="$t('Setting.InMostRecentBuild')" :column="1" class="mb-20">
                <a-descriptions-item :label="$t('Setting.User')">
                  {{ singleDict.dictKey }}
                </a-descriptions-item>
                <a-descriptions-item :label="$t('Setting.Time')">
                  {{ singleDict.createTime }}
                </a-descriptions-item>
                <a-descriptions-item :label="$t('Setting.Parameters')">
                  {{ singleDict.dictValue }}
                </a-descriptions-item>
                <a-descriptions-item :label="$t('Setting.Status')">
                  <a-tag v-if="singleDict.comment"
                    :color="singleDict.comment.indexOf('完成') !== -1 ? 'green' : singleDict.comment.indexOf('错误') !== -1 ? 'red' : 'orange'">
                    {{ singleDict.comment }}
                    <a-popconfirm :title="$t('Setting.sureChangeState')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                      @confirm="updateSingleDict(1, singleDict.id, $t('Setting.ManualClosing'))">
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
                      <a-col :span="8">
                        <a-form-model-item class="mb-10" :label="$t('Setting.StorageSpace')" :colon="false" prop="storageId">
                          <a-select @change="handleStorageChange" showSearch allowClear
                            v-model="buildGraphIndexForm.storageId" :placeholder="$t('Setting.selectTheStorageSpace')">
                            <a-select-option v-for="storageId in storages" :key="storageId">
                              {{ storageId }}
                            </a-select-option>
                          </a-select>
                        </a-form-model-item>
                      </a-col>
                      <a-col :span="8">
                        <a-form-model-item class="mb-10" :label="$t('Setting.OwnedWarehouse')" :colon="false" prop="repositoryId">
                          <a-select showSearch @change="handleRepositoryChange" allowClear
                            v-model="buildGraphIndexForm.repositoryId" :placeholder="$t('Setting.selectYourRepository')">
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
                            {{ $t('Setting.ProductAbsolutePath') }}
                            <a-popover placement="topLeft">
                              <template slot="content">
                                <p class="mb-0">{{ $t('Setting.specifyDirectoryBuildData') }}</p>
                                <p class="mb-0">{{ $t('Setting.DirectoryAbsolutePath') }}</p>
                              </template>
                              <a class="ml-5"><a-icon type="question-circle" theme="filled" /></a>
                            </a-popover>
                          </template>
                          <a-input v-model="buildGraphIndexForm.path" :placeholder="$t('Setting.enterAbsoluteProductPath')" />
                        </a-form-model-item>
                      </a-col>
                    </a-row>
                    <a-row :gutter="[24]">
                      <a-col :span="4">
                        <a-form-model-item class="mb-10" :label="$t('Setting.SynchronizingMetadata')" :colon="false" prop="metadata">
                          <a-switch v-model="buildGraphIndexForm.metadata"  />
                        </a-form-model-item>
                      </a-col>
                    </a-row>
                    <a-row :gutter="[24]">
                      <a-col :span="12">
                        <a-form-model-item :wrapper-col="{ span: 14, offset: 6 }">
                          <a-popconfirm :title="$t('Setting.sureBuildData')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                            @confirm="buildGraphIndexFormSubmit" :disabled="singleDict.comment.indexOf('中') > -1">
                            <a-button type="danger" :disabled="singleDict.comment.indexOf('中') > -1">
                              {{ $t('Setting.Save') }}
                            </a-button>
                          </a-popconfirm>
                          <a-button class="ml-10" @click="buildGraphIndexResetForm">
                            {{ $t('Setting.Cancel') }}
                          </a-button>
                        </a-form-model-item>
                      </a-col>
                    </a-row>
                  </a-col>
                </a-row>
              </a-form-model>
            </a-card>
          </a-tab-pane>
          <a-tab-pane :key="2" :tab="$t('Setting.MavenDataMigration')">
            <a-card :bordered="false" class="header-solid">
              <template #title>
                <h6>{{ $t('Setting.ArtifactDataMigration') }}</h6>
                <p>{{ $t('Setting.MavenIndxerSonatypeNexus') }}
                </p>
              </template>
              <data-migration/>
            </a-card>
          </a-tab-pane>
          <a-tab-pane :key="3" :tab="$t('Setting.BugUpdate')">
            <a-row :gutter="[24]">
              <a-col :span="12">
                <a-card :bordered="false" class="header-solid">
                  <template #title>
                    <h6>{{ $t('Setting.UpdateVulnerabilityData') }}</h6>
                    <p>{{ $t('Setting.updateDataToLibrary') }}
                    </p>
                  </template>
                  <a-descriptions :title="$t('Setting.TheLastUpdate')" :column="1" class="mb-20">
                    <a-descriptions-item :label="$t('Setting.User')">
                      {{ singleDict.dictKey }}
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.Time')">
                      {{ singleDict.createTime }}
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.Status')">
                      <a-tag v-if="singleDict.comment"
                        :color="singleDict.comment.indexOf('完成') !== -1 ? 'green' : singleDict.comment.indexOf('错误') !== -1 ? 'red' : 'orange'">
                        {{ singleDict.comment }}
                        <a-popconfirm :title="$t('Setting.sureChangeState')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                          @confirm="updateSingleDict(3, singleDict.id, $t('Setting.ManualClosing'))">
                          <a-icon type="unlock" theme="filled" v-if="singleDict.comment.indexOf('中') !== -1" />
                        </a-popconfirm>
                      </a-tag>
                      <span v-else>--</span>
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.Timing')">
                      <a-input size="small"
                        :placeholder="$t('Setting.CronRules')" v-model="vulnerabilityCron.cronExpression">
                        <a-icon slot="suffix" type="clock-circle"/>
                      </a-input>
                    </a-descriptions-item>
                  </a-descriptions>
                  <a-popconfirm :title="$t('Setting.SureUpdateTheVulnerabilityData')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                    @confirm="vulnerabilitiesSubmit(1)" :disabled="singleDict.comment.indexOf('中') > -1">
                    <a-button :disabled="singleDict.comment.indexOf('中') > -1" class="mr-20">
                      {{ $t('Setting.ManualUpdate') }}
                    </a-button>
                  </a-popconfirm>
                  <a-popconfirm :title="$t('Setting.SureUpdateTheVulnerabilityDataCron')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                    @confirm="vulnerabilitiesSubmit(2)" :disabled="singleDict.comment.indexOf('中') > -1">
                    <a-button type="primary" :disabled="singleDict.comment.indexOf('中') > -1" class="mr-20">
                      {{ $t('Setting.RegularUpdate') }}
                    </a-button>
                  </a-popconfirm>
                  <a-popconfirm :title="$t('Setting.SureDeleteTheVulnerabilityDataCron')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                    @confirm="delCronOne" :disabled="singleDict.comment.indexOf('中') > -1">
                    <a-button type="danger" :disabled="singleDict.comment.indexOf('中') > -1">
                      {{ $t('Setting.DeleteRegularUpdate') }}
                    </a-button>
                  </a-popconfirm>
                </a-card>
              </a-col>
              <a-col :span="12">
                <a-card :bordered="false" class="header-solid">
                  <template #title>
                    <h6>{{ $t('Setting.ScanSettings') }}</h6>
                    <p>{{ $t('Setting.ArtifactScanTips') }}
                    </p>
                  </template>
                  <a-descriptions :title="$t('Setting.TheLastScan')" :column="1" class="mb-20">
                    <a-descriptions-item :label="$t('Setting.User')">
                      {{ artifactScanDict.dictKey }}
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.Time')">
                      {{ artifactScanDict.createTime }}
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.ArtifactScanType')">
                      <span v-if="artifactScanDict.dictKey !== '--'">
                        {{ artifactScanDict.dictKey === 'Cron Job' ? $t('Setting.ScheduledScan') : $t('Setting.ManualScanning') }}
                      </span>
                      <span v-else>
                        --
                      </span>
                    </a-descriptions-item>
                    <a-descriptions-item :label="$t('Setting.Timing')">
                      <a-input size="small"
                        :placeholder="$t('Setting.CronRules')" v-model="artifactScanCron.cronExpression">
                        <a-icon slot="suffix" type="clock-circle"/>
                      </a-input>
                    </a-descriptions-item>
                  </a-descriptions>
                  <a-popconfirm :title="$t('Setting.ArtifactScanNow')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                    @confirm="artifactScanSubmit(1)">
                    <a-button class="mr-20">
                      {{ $t('Setting.ScanImmediately') }}
                    </a-button>
                  </a-popconfirm>
                  <a-popconfirm :title="$t('Setting.SureUpdateTheArtifactScanCron')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                    @confirm="artifactScanSubmit(2)">
                    <a-button type="primary" class="mr-20">
                      {{ $t('Setting.ScheduledScan') }}
                    </a-button>
                  </a-popconfirm>
                  <a-popconfirm :title="$t('Setting.SureDeleteTheArtifactScanCron')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                    @confirm="delArtifactCronOne">
                    <a-button type="danger">
                      {{ $t('Setting.DeleteRegularUpdate') }}
                    </a-button>
                  </a-popconfirm>
                </a-card>
              </a-col>
            </a-row>
          </a-tab-pane>
          <a-tab-pane :key="4" :tab="$t('Setting.BackupStrategy')">
            <BackupStrategy/>
          </a-tab-pane>
          <a-tab-pane :key="5" :tab="$t('Setting.CachingStrategy')">
            <a-card :bordered="false" class="header-solid">
              <template #title>
                <h6>{{ $t('Setting.CachingStrategy') }}</h6>
                <p>{{ $t('Setting.setSSDCacheAccelerationStrategy') }}</p>
              </template>
              <ArtifactsCache/>
            </a-card>
          </a-tab-pane>
          <a-tab-pane :key="6" :tab="'DB' + $t('Setting.Information')">
            <a-tabs class="tabs-sliding" :default-active-key="1" @change="dbTabChange($event)">
              <a-tab-pane :key="1" tab="Schema">
                <a-card :bordered="false" class="header-solid">
                  <template #title>
                    <h6>Schema {{ $t('Setting.Information') }}</h6>
                    <p>{{ $t('Setting.viewTheSchemaInformation') }}</p>
                  </template>
                  <a-row :gutter="[24]">
                    <a-col :span="24">
                      <a-tag color="#f50" class="mb-10 ml-15" v-if="janusGraphInfo.exceptionalIndexSet && janusGraphInfo.exceptionalIndexSet.length > 0">
                        {{ $t('Setting.Notice') }}， {{janusGraphInfo.exceptionalIndexSet }} {{ $t('Setting.checkIndexInformation') }}
                      </a-tag>
                      <prism-editor class="metadata-prism-editor" v-model="janusGraphInfo.schema"
                        :highlight="highlighterHandle" :line-numbers="true" :readonly="true">
                      </prism-editor>
                    </a-col>
                  </a-row>
                </a-card>
              </a-tab-pane>
              <a-tab-pane :key="2" :tab="$t('Setting.IndexingOperations')">
                <a-card :bordered="false" class="header-solid">
                  <template #title>
                    <h6>{{ $t('Setting.IndexingOperations') }}</h6>
                    <p>{{ $t('Setting.usedIndexReconstructionRegistration') }}</p>
                  </template>
                  <a-row :gutter="[24]">
                    <a-col :span="24">
                      <a-form-model layout="horizontal" ref="janusGraphIndexForm" :model="janusGraphIndexForm"
                        :rules="janusGraphIndexRules" :hideRequiredMark="false">
                        <a-row :gutter="[24]">
                          <a-col :span="24">
                            <a-row :gutter="[24]">
                              <a-col :span="6">
                                <a-form-model-item :label="$t('Setting.IndexName')" :colon="false" prop="indexName">
                                  <a-input :placeholder="$t('Setting.enterTheIndexName')" v-model="janusGraphIndexForm.indexName" />
                                </a-form-model-item>
                              </a-col>
                            </a-row>
                            <a-row :gutter="[24]">
                              <a-col :span="12">
                                <a-form-model-item>
                                  <a-popconfirm :title="$t('Setting.sureRebuildThisIndex')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                                    @confirm="janusGraphIndexFormSubmit(1)">
                                    <a-button type="danger" class="mr-20">
                                      {{ $t('Setting.Reindexing') }}
                                    </a-button>
                                  </a-popconfirm>
                                  <a-popconfirm :title="$t('Setting.sureRegisterThisIndex')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                                    @confirm="janusGraphIndexFormSubmit(2)">
                                    <a-button type="danger">
                                      {{ $t('Setting.RegisterTheIndex') }}
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
              <a-tab-pane :key="3" :tab="$t('Setting.InstanceManipulation')">
                <a-card :bordered="false" class="header-solid">
                  <template #title>
                    <h6>{{ $t('Setting.InstanceManipulation') }}</h6>
                    <p>{{ $t('Setting.viewAndDeleteInstance') }}</p>
                  </template>
                  <a-row :gutter="[24]">
                    <a-col :span="24">
                      <a-tag color="#f50" class="mb-10" v-if="janusGraphInfo.instanceStatus !== 'normal'">
                        {{ $t('Setting.clusterNodesNum') }}[{{janusGraphInfo.clusterNodeTotal}}]，{{ $t('Setting.instancesIsAbnormal') }}
                      </a-tag>
                    </a-col>
                    <a-col :span="6" v-for="(instance, index) in janusGraphInfo.openInstances" :key="index">
                      <a-tag :color="instance.indexOf('(current)') !== -1 ? 'red' : 'green'">
                        {{ instance }}
                        <a-popconfirm :title="$t('Setting.sureDeleteTheInstance')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
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
          <a-tab-pane :key="7" :tab="$t('Setting.SystemConfig')">
            <a-card :bordered="false" class="header-solid">
              <template #title>
                <h6>{{ $t('Setting.SystemConfig') }}</h6>
                <p>{{ $t('Setting.SystemConfigInfo') }}
                </p>
              </template>
              <system-config/>
            </a-card>
          </a-tab-pane>
          <a-tab-pane :key="8" :tab="$t('Setting.JfrogMigrate')">
            <a-card :bordered="false" class="header-solid">
              <template #title>
                <h6>{{ $t('Setting.JfrogMigrate') }}</h6>
                <p>{{ $t('Setting.JfrogMigrateInfo') }}
                </p>
              </template>
              <jfrog-migration/>
            </a-card>
            <a-card :bordered="false" class="header-solid mt-10" >
              <template #title>
                <h6>{{ $t('Setting.JforgArtifactMigrate') }}</h6>
                <p>{{ $t('Setting.JforgArtifactMigrateInfo') }}
                </p>
              </template>
              <migrate-artifact/>
            </a-card>
          </a-tab-pane>
          <a-tab-pane :key="9" :tab="$t('Setting.SystemImportExport')">
            <a-card :bordered="false" class="header-solid mt-10" >
              <template #title>
                <h6>{{ $t('Setting.SystemImportExport') }}</h6>
                <p>{{ $t('Setting.SystemImportExportInfo') }}
                </p>
              </template>
              <SystemImportExport/>
            </a-card>
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
import DataMigration from "./components/DataMigration.vue"
import ArtifactsBackup from "./components/ArtifactsBackup.vue";
import BackupStrategy from "./components/BackupStrategy/index.vue";
import ArtifactsCache from "./components/Cache/index.vue";
import JfrogMigration from "./components/JfrogMigration.vue"
import MigrateArtifact from "./components/MigrateArtifact.vue"
import SystemConfig from "./components/SystemConfig.vue"
import SystemImportExport from "./components/SystemImportExport/index.vue"

import {
  getStoragesAndRepositories,
  delCronOne
} from "@/api/folib"
import {
  buildGraphIndex
} from "@/api/artifact"
import {
  vulnerabilitiesDataUpdate,
  artifactScan,
  getCrontaskByClass
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
    const checkIndexName = (rule, value, callback) => {
      if (!value) {
        callback(new Error(this.$t('Setting.enterTheIndexName')))
      } else {
        callback()
      }
    }
    return {
      storages: [],
      repositoriesData: {},
      repositories: [],
      buildGraphIndexForm: {
        storageId: undefined,
        repositoryId: undefined,
        metadata: false,
        path: ''
      },
      janusGraphInfo: {
        openInstances: [],
        exceptionalIndexSet: [],
        clusterNodeTotal: 0,
        instanceStatus: '',
        schema: '',
      },
      janusGraphIndexForm: {
        indexName: "",
      },
      janusGraphIndexRules: {
        indexName: [
          { required: true, trigger: 'blur', validator: checkIndexName },
        ],
      },
      singleDict: {
        createTime: '--',
        dictKey: '--',
        dictValue: '--',
        comment: ''
      },
      artifactScanDict: {
        createTime: '--',
        dictKey: '--',
        dictValue: '--',
        comment: ''
      },
      vulnerabilityCron: {
        cronExpression: undefined,
        uuid: undefined,
      },
      artifactScanCron: {
        cronExpression: undefined,
        uuid: undefined,
      },
    }
  },
  components: {
    PrismEditor,
    DataMigration,
    ArtifactsBackup,
    ArtifactsCache,
    JfrogMigration,
    BackupStrategy,
    MigrateArtifact,
    SystemConfig,
    SystemImportExport,
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
        message = this.$t('Setting.OperationSuccessful')
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
              this.buildGraphIndexResetForm()
              setTimeout(() => {
                this.getSingleDict('build_graph_index')
              }, 100)
              this.message("success", this.$t('Setting.buildDataTaskHasStarted'))
            }
          }).catch((err) => {
            this.message("error", this.$t('Setting.FailExecuteBuildData'))
          }).finally(() => {

          })
        }
      })
    },
    buildGraphIndexResetForm() {
      this.$refs.buildGraphIndexForm.resetFields()
      this.initData()
    },
    vulnerabilitiesSubmit(type) {
      let params = {}
      if (type === 2 && !this.vulnerabilityCron.cronExpression) {
        this.message("warning", this.$t('Setting.CronRules'))
        return false
      }
      if (type === 2) {
        params.cron = this.vulnerabilityCron.cronExpression
      }
      vulnerabilitiesDataUpdate(params).then(res => {
        setTimeout(() => {
          this.getSingleDict('vulnerability_update')
        }, 100)
        if (type === 2) {
          this.message("success", this.$t('Setting.VulnerabilityUpdateCronTask'))
          this.getCrontaskByClass()
        } else {
          this.message("success", this.$t('Setting.vulnerabilityUpdateTaskStarted'))
        }
      }).catch((err) => {
        this.message("error", this.$t('Setting.updateFailExecute'))
      }).finally(() => {

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
          this.message("warning", this.$t('Setting.currentInstanceNotAllowDeleted'))
          return false
        }
        deleteInstance(instance).then(res => {
          this.message("success", this.$t('Setting.DeleteInstanceSuccessful'))
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
              this.message("success", this.$t('Setting.reindexingWasSuccessful'))
            }).catch((err) => {
              let msg = err.response.data.error ? err.response.data.error : this.$t('Setting.FailReindex')
              this.message("error", msg)
            }).finally(() => {
            })
          } else if (type === 2) {
            registerIndex(data).then(res => {
              this.message("success", this.$t('Setting.RegisterIndexSuccessful'))
            }).catch((err) => {
              let msg = err.response.data.error ? err.response.data.error : this.$t('Setting.FailRegisterTheIndex')
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
      } else if (activeTab === 3) {
        this.getCrontaskByClass()
        this.getSingleDict('vulnerability_update')
        this.getArtifactScanDict()
      } else if (activeTab === 6) {
        this.getJanusGraphInfo()
      }
    },
    dbTabChange(active) {
      if (active === 1 || active === 3) {
        this.getJanusGraphInfo()
      } else if (active === 2) {
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
    getArtifactScanDict() {
      this.artifactScanDict = {
        createTime: '--',
        dictKey: '--',
        dictValue: '--',
        comment: ''
      }
      getSingleDict({ dictType: 'artifact_full_scan' }).then(res => {
        if (res) {
          this.artifactScanDict = res
        }
      })
    },
    updateSingleDict(type, id, comment) {
      updateSingleDict({ id: id, comment: comment }).then(res => {
        this.tabChange(type)
        this.$message("success", this.$t('Setting.updateStatusSuccessful'))
      })
    },
    getCrontaskByClass() {
      getCrontaskByClass({ className: 'com.veadan.folib.cron.jobs.VulnerabilityRefreshCronJob' }).then(res => {
        if (res && res.cronTaskConfigurations && res.cronTaskConfigurations.length > 0) {
          this.vulnerabilityCron.uuid = res.cronTaskConfigurations[0].uuid
          this.vulnerabilityCron.cronExpression = res.cronTaskConfigurations[0].cronExpression
        }
      })
      getCrontaskByClass({ className: 'com.veadan.folib.cron.jobs.ArtifactScanCronJob' }).then(res => {
        if (res && res.cronTaskConfigurations && res.cronTaskConfigurations.length > 0) {
          this.artifactScanCron.uuid = res.cronTaskConfigurations[0].uuid
          this.artifactScanCron.cronExpression = res.cronTaskConfigurations[0].cronExpression
        }
      })
    },
    delCronOne() {
      if (!this.vulnerabilityCron.uuid) {
         return false
      }
      delCronOne(this.vulnerabilityCron.uuid).then(res => {
        this.vulnerabilityCron = {
          uuid: undefined,
          cronExpression: undefined,
        }
        this.message("success", this.$t('Setting.OperationSuccessful'))
      })
    },
    artifactScanSubmit(type) {
      let params = {}
      if (type === 2 && !this.artifactScanCron.cronExpression) {
        this.message("warning", this.$t('Setting.CronRules'))
        return false
      }
      if (type === 2) {
        params.cron = this.artifactScanCron.cronExpression
      }
      artifactScan(params).then(res => {
        setTimeout(() => {
          this.getArtifactScanDict()
        }, 100)
        if (type === 2) {
          this.message("success", this.$t('Setting.ArtifactScanCronTask'))
          this.getCrontaskByClass()
        } else {
          this.message("success", this.$t('Setting.OperationSuccessful'))
        }
      }).catch((err) => {
        this.message("error", this.$t('Setting.updateFailExecute'))
      }).finally(() => {

      })
    },
    delArtifactCronOne() {
      if (!this.artifactScanCron.uuid) {
         return false
      }
      delCronOne(this.artifactScanCron.uuid).then(res => {
        this.artifactScanCron = {
          uuid: undefined,
          cronExpression: undefined,
        }
        this.message("success", this.$t('Setting.OperationSuccessful'))
      })
    },
  }
}
</script>

<style lang="scss" scoped></style>
