<template>
    <div>
        <a-row type="flex" :gutter="24">
            <a-col :span="24" class="mb-24">
                <a-card :bordered="false" class="header-solid" :body-style="{padding:0,paddingBottom:'16px'}"
                        :title="$t('FederalPromotionPolicy.CardTitle')">
                    <div slot="extra">
                        <a-tooltip @click="showBlockStrategyInfo()">
                            <template slot="title">
                                <span>{{ $t('FederalPromotionPolicy.AddTitle') }}</span>
                            </template>
                            <a-icon type="plus-circle" theme="filled" class="cursor-pointer package-name-add mr-10"
                                    :style="{ fontSize: '28px', color: '#1890FF' }"/>
                        </a-tooltip>
                        <a-input-search class="v-search" v-model="federalPromotionPolicyQuery.name"
                                        @search="handeSearch"/>
                    </div>
                    <a-table :columns="columns" :data-source="federalPromotionPolicyData" :scroll="{ x: true }"

                             @change="handleChangeTable" :loading="federalPromotionPolicyLoading"
                             :row-key="(r, i) => i.toString()"
                             :pagination="{ pageSize: federalPromotionPolicyQuery.pageSize, current: federalPromotionPolicyQuery.pageNumber, total: federalPromotionPolicyQuery.total, showLessItems: true }">
                        <span slot="isEnabled" slot-scope="text, record">
                            <a-tag :color="text===true?'green':'gray'">
                                 {{
                                    text === true ? $t('FederalPromotionPolicy.computing') : $t('FederalPromotionPolicy.disable')
                                }}
                            </a-tag>
                        </span>
                        <span slot="sourceRepositories" slot-scope="text, record">

                            <span v-if="record.sourceRepositories" @click="showTargetRepositories(record.sourceRepositories)">
                                {{record.sourceRepositories.length + ' ' + $t('FederalPromotionPolicy.Total')}}
                                |
                                <span v-for="(item, i) in record.sourceRepositories" :key="i">
                                  {{ item.storageId + ':' + item.repositoryId}}
                                  <span v-if="i!=record.sourceRepositories.length-1">,</span>
                                </span>
                             </span>
                        </span>
                        <span slot="targetRepositories" slot-scope="text, record">
                              <span v-if="record.targetRepositories" @click="showTargetRepositories(record.targetRepositories)">
                                {{record.targetRepositories.length + ' ' + $t('FederalPromotionPolicy.Total')}}
                                |
                                <span v-for="(item, i) in record.targetRepositories" :key="i">
                                  {{ item.storageId + ':' + item.repositoryId}}
                                  <span v-if="i!=record.targetRepositories.length-1">,</span>
                                </span>
                             </span>
                        </span>
                        <div slot="operation" slot-scope="text, record">
                            <div class="col-action">
                                <a-popconfirm :title="$t('Package.SureDelete')" okType="danger"
                                              :ok-text="$t('Package.Confirm')"
                                              @confirm="federalPromotionPolicyDelete(record)"
                                              :cancel-text="$t('Package.Cancel')">
                                    <a-button type="link" size="small" >
                                        <svg width="16" height="16" viewBox="0 0 20 20" fill="none"
                                             xmlns="http://www.w3.org/2000/svg">
                                            <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                                                  d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                                                  fill="#111827"/>
                                        </svg>
                                        <span class="text-danger">DELETE</span>
                                    </a-button>
                                </a-popconfirm>
                                <a-button type="link" size="small" @click="showFederalPromotionPolicyDatils(record)">
                                    <svg width="16" height="16" viewBox="0 0 20 20" fill="none"
                                         xmlns="http://www.w3.org/2000/svg">
                                        <path class="fill-muted"
                                              d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                                              fill="#111827"/>
                                        <path class="fill-muted"
                                              d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                                              fill="#111827"/>
                                    </svg>
                                    <span class="text-dark">EDIT</span>
                                </a-button>
                            </div>
                        </div>
                    </a-table>


                </a-card>
            </a-col>
        </a-row>

        <a-modal v-model="showTargetRepositoriesModal" :footer="null" :forceRender="true" on-ok="showTargetRepositoriesModal = false">
            <a-list item-layout="vertical" size="large" :data-source="targetRepositoriesList"
                    :pagination="targetRepositoriesList.length === 0 ? false : { pageSize: 8, total: targetRepositoriesList.length, showLessItems: true, showTotal:total =>  ` ${total} ` + ' '+ $t('FederalPromotionPolicy.Total')}">
                <a-list-item slot="renderItem" :key="index" slot-scope="item, index">
                    <div>
                        <a-tag class="mb-5 bg-warning">{{$t('FederalPromotionPolicy.StorageId')}}</a-tag>
                        <span>{{ item.storageId }}</span>
                    </div>
                    <div>
                        <a-tag class="mb-5 bg-success">{{$t('FederalPromotionPolicy.RepositoryId')}}</a-tag>
                        <span>{{ item.repositoryId }}</span>
                    </div>
                </a-list-item>
            </a-list>
        </a-modal>

        <a-drawer
            :title="$t('FederalPromotionPolicy.DrawerTitle')"
            placement="right"
            width="60%"
            :visible="visibleDrawer"
            :body-style="{ paddingBottom: '80px' }"
            @close="onClose"
        >
            <a-card :bordered="false" class="header-solid h-full card-profile-information"
                    :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }" :headStyle="{ paddingRight: 0 }">
                <a-form-model class="ant-advanced-search-form"
                              ref="federalPromotionPolicyFormRef"
                              :model="federalPromotionPolicyForm"
                              :rules="federalPromotionPolicyFormRules"
                >
                    <a-row :gutter="24">
                        <a-col :md="10" :sm="24">
                            <a-form-model-item :label="$t('FederalPromotionPolicy.PolicyName')"
                                               class="no-colon"
                                               prop="name"
                                               style="padding-bottom: 0;margin-bottom: 0">
                                <a-input style="width: 80%;margin: 0 8px 8px 0;"
                                         :allowClear="true"
                                         v-model="federalPromotionPolicyForm.name"
                                         :placeholder="$t('FederalPromotionPolicy.PleaseInput')+''+$t('FederalPromotionPolicy.PolicyName')"/>
                            </a-form-model-item>

                        </a-col>
                        <a-col :md="6" :sm="24">
                            <a-form-model-item  :label="$t('FederalPromotionPolicy.IsEnabled')"
                                               style="padding-bottom: 0;margin-bottom: 0">
                                <a-switch v-model="federalPromotionPolicyForm.isEnabled" />
                            </a-form-model-item>
                        </a-col>
                      <a-col :md="6" :sm="24">
                        <a-form-model-item  :label="$t('FederalPromotionPolicy.IsDeleteSync')"
                                            style="padding-bottom: 0;margin-bottom: 0">
                          <a-switch v-model="federalPromotionPolicyForm.isDeleteSync" />
                        </a-form-model-item>
                      </a-col>
                    </a-row>

                    <a-row :gutter="24">
                        <a-col :md="24" :sm="24">
                            <a-form-model-item
                                style="padding-bottom: 0;margin-bottom: 0;display: flex; align-items: center;">
                                 <span slot="label" style="margin: 0;padding: 0;  display: inline-block; ">
                                   {{ $t('FederalPromotionPolicy.PathRule') }}
                                    <a-popover placement="topLeft">
                                          <template slot="content">
                                            <p class="mb-0">{{ $t('UnionRepository.ArtifactPathTips1') }}</p>
                                            <p class="mb-0">{{ $t('UnionRepository.ArtifactPathTips2') }}</p>
                                            <p class="mb-0">{{ $t('UnionRepository.ArtifactPathTips3') }}</p>
                                            <p class="mb-0">{{ $t('UnionRepository.ArtifactPathTips4') }}</p>
                                          </template>
                                          <a class="ml-5"><a-icon type="question-circle" theme="filled"/></a>
                                      </a-popover>
                                  </span>
                                <span style="margin: 0;padding: 0; display: inline-block; ">
                                   <a-tooltip :title="$t('FederalPromotionPolicy.AddPathTitle')">
                                        <a-icon type="plus-circle" class="dynamic-add-button" @click="addPathDomain"/>
                                   </a-tooltip>
                                </span>

                            </a-form-model-item>
                            <a-form-model-item
                                v-for="(path,index) in federalPromotionPolicyForm.paths"
                                :key="path.key"
                                style="padding-bottom: 0;margin-bottom: 0"
                            >
                                <a-input v-model="path.value"
                                         :placeholder="$t('FederalPromotionPolicy.PleaseInput')+''+$t('FederalPromotionPolicy.PathRule')"
                                         style="width: 60%;margin: 0 8px 8px 0;"
                                />
                                <a-icon
                                    class="dynamic-delete-button" style="  left: 2%;"
                                    type="minus-circle-o"
                                    v-if="federalPromotionPolicyForm.paths.length>1"
                                    :disabled="federalPromotionPolicyForm.paths.length === 1"
                                    @click="removePathDomain(path)"

                                />
                            </a-form-model-item>

                        </a-col>
                    </a-row>
                    <a-row :gutter="24">
                        <a-col :md="24" :sm="24">

                            <a-form-item style="padding-bottom: 0;margin-bottom: 0;display: flex; align-items: center;">
                                 <span slot="label" style="margin: 0;padding: 0;  display: inline-block;">
                                   {{ $t('FederalPromotionPolicy.MetaDataRule') }}
                                 <a-popover placement="topLeft">
                                      <template slot="content">
                                        <p class="mb-0">{{ $t('UnionRepository.MetadataTips1') }}</p>
                                        <p class="mb-0">{{ $t('UnionRepository.MetadataTips2') }}</p>
                                      </template>
                                      <a class="ml-5"><a-icon type="question-circle" theme="filled"/></a>
                                  </a-popover>
                                 </span>
                                <span style="margin: 0;padding: 0; display: inline-block;">
                                     <a-tooltip :title="$t('FederalPromotionPolicy.AddMetaDataTitle')">
                                      <a-icon type="plus-circle" class="dynamic-add-button" @click="addMetaDataDomain"/>
                                    </a-tooltip>
                                </span>
                            </a-form-item>
                            <a-form-item style="padding-bottom: 0;margin-bottom: 0"
                                         v-for="(metaData,index) in federalPromotionPolicyForm.metaDatas"
                                         :key="index">
                                <a-input style="width: 30%;margin: 0 8px 8px 0;"
                                         :placeholder="$t('FederalPromotionPolicy.PleaseInput')+''+$t('FederalPromotionPolicy.MetaDataRule')+'key'"
                                         v-model="metaData[0].value"/>
                                <a-input style="width: 30%;margin: 0 8px 8px 0; left: 102px;"
                                         :placeholder="$t('FederalPromotionPolicy.PleaseInput')+''+$t('FederalPromotionPolicy.MetaDataRule')+'value'"
                                         v-model="metaData[1].value"/>
                                <a-icon
                                    class="dynamic-delete-button" style="left: 12%;"
                                    type="minus-circle-o"
                                    v-if="federalPromotionPolicyForm.metaDatas.length>1"
                                    :disabled="federalPromotionPolicyForm.metaDatas.length === 1"
                                    @click="removeMetaDataDomain(metaData)"
                                />
                            </a-form-item>

                        </a-col>
                    </a-row>

                    <a-row :gutter="24">
                        <a-col :md="24" :sm="24">
                            <a-form-model-item class="mb-10"
                                               :label="$t('FederalPromotionPolicy.SelectSourceRepositories')"
                                               :colon="false"
                                               prop="SourceRepositoriesKeys">
                                <a-transfer
                                    :data-source="repositoriesList"
                                    :titles="[$t('FederalPromotionPolicy.SelectableRepositories'), $t('FederalPromotionPolicy.SelectedRepositories')]"
                                    :render="item => item.title"
                                    :target-keys="selectedSourceRepositoriesKeys"
                                    :disabled="false"
                                    @change="sourceRepositoriesOnChange"
                                    @scroll="handleSourceRepositoriesScroll"
                                    @selectChange="selectSourceRepositoriesChange"
                                    show-search
                                    show-select-all
                                    :list-style="{ width: '450px', height: '350px', }">
                                </a-transfer>
                            </a-form-model-item>
                        </a-col>
                    </a-row>
                    <a-row :gutter="24">
                        <a-col :md="8" :sm="24">
                            <a-form-model-item :label="$t('FederalPromotionPolicy.SelectNodeType')"
                                               style="padding-bottom: 0;margin-bottom: 0">
                                <a-radio-group v-model="federalPromotionPolicyForm.nodeType" @change="onNodeChange">
                                    <a-radio :value="1">
                                        {{ $t('FederalPromotionPolicy.InternalNode') }}
                                        <a-popover placement="topLeft">
                                            <template slot="content">
                                                <p class="mb-0">{{instanceName + $t('UnionRepository.ArtifactRepositoryNode')}}</p>
                                            </template>
                                            <a class="ml-5">
                                                <a-icon type="question-circle" theme="filled"/>
                                            </a>
                                        </a-popover>
                                    </a-radio>
                                    <a-radio :value="2" style="left: 15%">
                                        {{ $t('FederalPromotionPolicy.ExternalNode') }}
                                        <a-popover placement="topLeft">
                                            <template slot="content">
                                                <p class="mb-0">{{ $t('UnionRepository.OtherType')  + $t('UnionRepository.ArtifactRepositoryNode') }}</p>
                                            </template>
                                            <a class="ml-5">
                                                <a-icon type="question-circle" theme="filled"/>
                                            </a>
                                        </a-popover>
                                    </a-radio>
                                </a-radio-group>
                            </a-form-model-item>

                        </a-col>
                        <a-col :md="16" :sm="24">
                            <a-form-item :label="$t('FederalPromotionPolicy.SelectingTargetNode')"
                                         :colon="false"
                                         prop="selectTargetNodes"
                                         style="padding-bottom: 0;margin-bottom: 0">
                                <a-select
                                    label-in-value
                                    v-model="federalPromotionPolicyForm.selectTargetNodes"
                                    :options="targetNodesOptions"
                                    style="width: 52%"
                                    :placeholder="$t('FederalPromotionPolicy.SelectingTargetNode')"
                                    @change="handleNodeChange"
                                >
                                    <a-select-option v-for="item in targetNodesOptions" :key="item.key"
                                                     :value="item.key">
                                        {{ item.label }}
                                    </a-select-option>

                                </a-select>
                            </a-form-item>
                        </a-col>
                    </a-row>
                    <a-row :gutter="24">
                        <a-col :md="24" :sm="24">
                            <a-form-model-item class="mb-10"
                                               :label="$t('FederalPromotionPolicy.SelectionTargetRepositories')"
                                               prop="TargetRepositoriesKeys"
                                               :colon="false">
                                <a-transfer
                                    :titles="[$t('FederalPromotionPolicy.SelectableRepositories'), $t('FederalPromotionPolicy.SelectedRepositories')]"
                                    :data-source="selectedTargetRepositories"
                                    :render="item => item.title"
                                    :target-keys="selectedTargetRepositoriesKeys"
                                    :disabled="false"
                                    @change="targetRepositoriesOnChange"
                                    @scroll="handleTargetRepositoriesScroll"
                                    @selectChange="selectTargetRepositoriesChange"
                                    show-search
                                    show-select-all
                                    :list-style="{ width: '450px', height: '350px', }">
                                </a-transfer>
                            </a-form-model-item>
                        </a-col>
                    </a-row>
                    <a-row :gutter="24">
                        <a-form-model-item>
                            <a-col :md="12" :sm="24" class="container">
                                <a-button type="primary" @click="federalPromotionPolicyFormSubmit">
                                    {{ $t('Setting.Save') }}
                                </a-button>
                            </a-col>
                            <a-col :md="12" :sm="24">
                                <a-button class="ml-10" @click="federalPromotionPolicyClose">
                                    {{ $t('Setting.Cancel') }}
                                </a-button>
                            </a-col>
                        </a-form-model-item>
                    </a-row>
                </a-form-model>
            </a-card>
        </a-drawer>
    </div>

</template>

<script>
import {
    federalPromotionPolicyQuery,
    federalPromotionPolicyAdd,
    federalPromotionPolicyEdit, federalPromotionPolicyDelete, federalPromotionPolicyDetail
} from "@/api/federalPromotionPolicy";
import {getArtifactDispatchStoragesAndRepositories, queryRepositories, queryRepositoriesByStorage} from "@/api/folib";
import {getExternalNodeRepositories} from "@/api/externalNode";

export default {
    data() {
        const checkRepositoriesKeys1 = (rule, value, callback) => {
            if (!this.selectedSourceRepositoriesKeys || this.selectedSourceRepositoriesKeys.length <= 0) {
                callback(new Error(this.$t('FederalPromotionPolicy.SelectSourceRepositories')))
            } else {
                callback()
            }
        }
        const checkRepositoriesKeys2 = (rule, value, callback) => {
            if (!this.selectedTargetRepositoriesKeys || this.selectedTargetRepositoriesKeys.length <= 0) {
                callback(new Error(this.$t('FederalPromotionPolicy.SelectionTargetRepositories')))
            } else {
                callback()
            }
        }

        const checkName = (rule, value, callback) => {

            if (!this.federalPromotionPolicyForm.name) {
                let mess = this.$t('FederalPromotionPolicy.PleaseInput');
                let mess1 = this.$t('FederalPromotionPolicy.PolicyName');
                mess = mess+mess1;
                callback(new Error(mess))
            } else {
                callback()
            }
        }

        return {

            federalPromotionPolicyData: [],
            columns: [
                {
                    title: '策略名',
                    i18nKey: 'FederalPromotionPolicy.PolicyName',
                    dataIndex: 'name',
                    key: 'name',
                    width: 160,
                },
                {
                    title: '启用状态',
                    i18nKey: 'FederalPromotionPolicy.IsEnabled',
                    dataIndex: 'isEnabled',
                    key: 'isEnabled',
                    width: 140,
                    scopedSlots: {customRender: 'isEnabled'},
                },
                {
                    title: '源仓库信息',
                    i18nKey: 'FederalPromotionPolicy.SourceRepositories',
                    dataIndex: 'sourceRepositories',
                    key: 'sourceRepositories',
                    ellipsis: true,
                    width: 160,
                    scopedSlots: {customRender: 'sourceRepositories'},
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
                    title: '目标仓库信息',
                    i18nKey: 'FederalPromotionPolicy.TargetRepositories',
                    dataIndex: 'targetRepositories',
                    key: 'targetRepositories',
                    ellipsis: true,
                    width: 160,
                    scopedSlots: {customRender: 'targetRepositories'},
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
                    title: '创建时间',
                    i18nKey: 'FederalPromotionPolicy.CreatedTime',
                    dataIndex: 'createdTime',
                    key: 'createdTime',
                    width: 200,
                },
                {
                    title: '创建人',
                    i18nKey: 'FederalPromotionPolicy.CreatedBy',
                    dataIndex: 'createdBy',
                    key: 'createdBy',
                    width: 140,
                },
                {
                    title: '操作',
                    i18nKey: 'Operate',
                    dataIndex: 'operation',
                    width: 180,
                    align: 'right',
                    scopedSlots: {customRender: 'operation'},
                }
            ],
            federalPromotionPolicyQuery: {
                pageNumber: 1,
                pageSize: 10,
                total: 0,
                name: undefined
            },
            federalPromotionPolicyLoading: false,
            visibleDrawer: false,
            federalPromotionPolicyFormRules: {
                name: [
                    { required: true, trigger: ['blur'], validator: checkName },
                ],
                paths: [
                    { required: false, trigger: ['blur', 'change'] },
                ],
                metaDatas: [
                    { required: false, trigger: ['blur', 'change'] },
                ],
                isEnabled: [
                    { required: false, trigger: ['blur', 'change'] },
                ],
                selectTargetNodes: [
                    { required: false, trigger: ['blur', 'change'] },
                ],
               SourceRepositoriesKeys: [
                    { required: true, trigger: ['blur', 'change'], validator: checkRepositoriesKeys1},
                ],
                TargetRepositoriesKeys: [
                    { required: true, trigger: ['blur', 'change'], validator: checkRepositoriesKeys2},
                ],
            },
            federalPromotionPolicyForm: {
                policyId:'',
                name: undefined,
                paths: [{
                    value: '',
                    key: Date.now(),
                }],
                metaDatas: [[
                    {
                        value: '',
                        key: Date.now(),
                    },
                    {
                        value: '',
                        key: Date.now(),
                    }
                ]],
                isEnabled: false,
                isDeleteSync: false,
                sourceRepositories: [],
                targetRepositories: [],
                nodeType: 1,
                selectTargetNodes: '',
            },
            //nodeType: 1,
            //selectTargetNodes: '',
            targetNodesOptions: [],
            selectedSourceRepositoriesKeys: [],
            targetNodesRepositories: [],
            selectedTargetRepositories: [],
            selectedTargetRepositoriesKeys: [],
            externalNodeRepositories:[],
            repositoriesLoading: false,
            repositoriesHasMore: false,
            repositoriesTargetLoading: false,
            repositoriesTargetHasMore: false,
            repositoriesPage: 1,
            repositoriesLimit: 100000,
            repositoriesTotal: 0,
            repositoriesList: [],
            subLayout: undefined,
            layout: undefined,
            policy: undefined,
            type: 'hosted',
            instanceName:'',
            showTargetRepositoriesModal: false,
            targetRepositoriesList:[],
        }
    },
    computed: {
        i18nBlockStrategyColumns() {
            return this.columns.map(column => {
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
        'layout': function (newval,oldVal) {
          if(this.federalPromotionPolicyForm.nodeType === 1){
            this.getTargetRepositories(this.type, newval, this.policy);
            if(this.targetNodesOptions && this.targetNodesOptions.length>0){
              this.handleDefNode({ value: this.targetNodesOptions[0].key,
                key: this.targetNodesOptions[0].key});
              this.handleTargetRepositories(this.targetNodesOptions[0].key);
            }
          }


        },
        'policy': function (newval,oldVal) {
          if(this.federalPromotionPolicyForm.nodeType === 1){
            this.getTargetRepositories(this.type, this.layout, newval);
            if(this.targetNodesOptions && this.targetNodesOptions.length>0){
              this.handleDefNode({ value: this.targetNodesOptions[0].key,
                key: this.targetNodesOptions[0].key});
              this.handleTargetRepositories(this.targetNodesOptions[0].key);
            }
          }
        },
      'selectedTargetRepositoriesKeys': function (newval,oldVal) {
        if(!newval ||  newval.length===0){
          this.subLayout= undefined;
          this.layout= undefined;
          this.policy=undefined;
        }
      },

    },
    methods: {
        successMsg(message) {
            if (!message) {
                message = this.$t('FederalPromotionPolicy.OperationSuccessful');
            }
            this.$notification["success"]({
                message: message,
                description: "",
            });
        },
        initData() {
            this.queryFederalPromotionPolicy();
            this.queryRepositoriesByStorage();
            this.getTargetRepositories(this.type, this.layout, this.policy);
            this.instanceName = sessionStorage.getItem("instanceName")
        },

        handleChangeTable(pagination, filters, sorter) {
            if (pagination) {
                this.federalPromotionPolicyLoading = true
                this.federalPromotionPolicyQuery.pageNumber = pagination.current
            }
            this.queryFederalPromotionPolicy()
        },

        queryFederalPromotionPolicy() {

            federalPromotionPolicyQuery(this.federalPromotionPolicyQuery).then(res => {
                this.federalPromotionPolicyData = []
                if (res) {
                    this.federalPromotionPolicyData = res.data.rows
                    this.federalPromotionPolicyQuery.total = res.data.total
                }
            }).finally(() => {
                this.federalPromotionPolicyLoading = false
            })
        },

        handeSearch() {
            this.federalPromotionPolicyQuery.page = 1
            this.queryFederalPromotionPolicy()
        },

        onClose() {
            this.federalPromotionPolicyClose();
        },

        showBlockStrategyInfo() {
            this.visibleDrawer = true
        },

        showDrawer() {
            this.visibleDrawer = true;
        },
        addPathDomain() {
            this.federalPromotionPolicyForm.paths.push({
                value: '',
                key: Date.now(),
            });
        },

        addMetaDataDomain() {
            this.federalPromotionPolicyForm.metaDatas.push([{
                value: '',
                key: Date.now(),
            }, {
                value: '',
                key: Date.now(),
            }]);
        },

        removePathDomain(item) {
            let index = this.federalPromotionPolicyForm.paths.indexOf(item);
            if (index !== -1) {
                this.federalPromotionPolicyForm.paths.splice(index, 1);
            }
        },

        removeMetaDataDomain(item) {
            let index = this.federalPromotionPolicyForm.metaDatas.indexOf(item);
            if (index !== -1) {
                this.federalPromotionPolicyForm.metaDatas.splice(index, 1);
            }
        },

        getTargetRepositories(type, layout, policy) {
            getArtifactDispatchStoragesAndRepositories({
                type: type,
                layout: layout,
                policy: policy,
            }).then((res) => {
                this.targetNodesRepositories = []
                this.targetNodesOptions = []
                res.forEach((item) => {
                    if (item.children && item.children.length > 0) {
                        this.targetNodesRepositories.push(item)
                    }
                    let temp = {};
                    temp.key = item.key
                    temp.label = item.name
                    this.targetNodesOptions.push(temp);
                })

              if(this.targetNodesOptions){
                this.handleDefNode({ value: this.targetNodesOptions[0].key,
                  key: this.targetNodesOptions[0].key});
                this.handleTargetRepositories(this.targetNodesOptions[0].key);
              }
            })
        },

        handleTargetRepositories(key) {
            this.selectedTargetRepositories = [];
            this.targetNodesRepositories.forEach((item) => {
                if (item.key === key) {
                    if (item.children && item.children.length > 0) {
                        item.children.forEach((item2) => {
                            if (item2.children && item2.children.length > 0) {
                                item2.children.forEach((item3) => {
                                    if (item3.layout === this.layout) {
                                        let temp = {}
                                        temp.key = `${item2.id}:${item3.id}`
                                        temp.title = temp.key
                                        temp.label = temp.key
                                        temp.subLayout = item3.subLayout
                                        temp.policy = item3.policy
                                        temp.layout = item3.layout
                                        this.selectedTargetRepositories.push(temp)
                                    }else if(!this.layout){
                                      let temp = {}
                                      temp.key = `${item2.id}:${item3.id}`
                                      temp.title = temp.key
                                      temp.label = temp.key
                                      temp.subLayout = item3.subLayout
                                      temp.policy = item3.policy
                                      temp.layout = item3.layout
                                      this.selectedTargetRepositories.push(temp)
                                    }
                                })
                            }
                        })
                    }
                }
            })
        },

        handleNodeChange(value, option) {
            const key = value.key
            this.handleTargetRepositories(key);
        },

        queryRepositoriesByStorage(isLoadMore) {
            if (!isLoadMore) {
                this.repositoriesPage = 1
                this.repositoriesList = []
                this.repositoriesHasMore = true
            }
            if (!this.repositoriesHasMore || this.repositoriesLoading) {
                return
            }
            this.repositoriesLoading = true;
            queryRepositories({page: this.repositoriesPage, limit: this.repositoriesLimit}).then(res => {
                const newRepositories = res.data.rows.map((item) => {
                    let temp = {}
                    temp.key = `${item.storageId}:${item.id}`
                    temp.title = temp.key
                    temp.label = temp.key
                    temp.subLayout = item.subLayout
                    temp.policy = item.policy
                    temp.layout = item.layout
                    return temp
                })
                this.repositoriesList = [...this.repositoriesList, ...newRepositories]
                this.repositoriesTotal = res.data.total
                this.repositoriesHasMore = this.repositoriesList.length < this.repositoriesTotal
                this.repositoriesPage++
                this.selectedSourceRepositoriesKeys.forEach(key => {
                    if (!this.repositoriesList.some(use => use.key === key)) {
                        let temp = {}
                        temp.key = key
                        temp.title = temp.key
                        temp.label = temp.key
                        temp.subLayout = item.subLayout
                        temp.policy = item.policy
                        temp.layout = item.layout
                        this.repositoriesList.push(temp)
                    }
                });
            }).catch(error => {
                console.error('Error fetching repositories:', error);
            }).finally(() => {
                this.repositoriesLoading = false;
            });
        },

        sourceRepositoriesOnChange(nextTargetKeys) {
            this.selectedSourceRepositoriesKeys = nextTargetKeys.filter(item => this.repositoriesList.some(data => data.key === item && data.policy === this.policy && data.subLayout === this.subLayout))
            //console.log(this.selectedSourceRepositoriesKeys)
        },


        handleSourceRepositoriesScroll(direction, e) {
            if (direction === 'left') {
                this.$nextTick(() => {
                    const target = e.target
                    const {scrollTop, clientHeight, scrollHeight} = target;
                    if (scrollTop + clientHeight >= scrollHeight - 50) {
                        if (!this.repositoriesLoading && this.repositoriesHasMore) {
                            this.queryRepositoriesByStorage(true)
                        }
                    }
                })
            }
        },

        selectSourceRepositoriesChange(sourceSelectedKeys, targetSelectedKeys) {
            if (this.subLayout === undefined && this.policy === undefined) {
                if (sourceSelectedKeys.length > 0) {
                    this.repositoriesList.forEach(item => {
                        const sourceRepositories = sourceSelectedKeys[0]
                        if (item.key === sourceRepositories) {
                            this.subLayout = item.subLayout;
                            this.policy = item.policy;
                            this.layout = item.layout;
                        }
                    });
                }
            }

        },

        targetRepositoriesOnChange(nextTargetKeys, direction, moveKeys) {
          this.selectedTargetRepositoriesKeys = nextTargetKeys;
          if (this.layout === undefined && nextTargetKeys.length>0) {
            if (nextTargetKeys.length > 0) {
              const sourceRepositories = nextTargetKeys[0]
              this.selectedTargetRepositories.forEach(item => {
                if (item.key === sourceRepositories) {
                  this.subLayout = item.subLayout;
                  this.policy = item.policy;
                  this.layout = item.layout;
                }
              });
            }
          }
        },

        handleTargetRepositoriesScroll(direction, e) {
            if (direction === 'left') {
                this.$nextTick(() => {
                    const target = e.target
                    const {scrollTop, clientHeight, scrollHeight} = target;
                    if (scrollTop + clientHeight >= scrollHeight - 50) {
                        if (!this.repositoriesTargetLoading && this.repositoriesTargetHasMore) {
                            this.handleTargetRepositories(this.federalPromotionPolicyForm.selectTargetNodes)
                        }
                    }
                })

            }
        },


      selectTargetRepositoriesChange(sourceSelectedKeys, targetSelectedKeys) {
      },

        federalPromotionPolicyFormSubmit() {
            this.$refs.federalPromotionPolicyFormRef.validate(valid => {
                if (valid) {

                    let dataForm = Object.assign({}, this.federalPromotionPolicyForm)
                    let data = {}
                    data.name= dataForm.name;
                    data.isEnabled = dataForm.isEnabled;
                    data.isDeleteSync = dataForm.isDeleteSync;
                    data.sourceRepositories = this.selectedSourceRepositoriesKeys.map(item => {
                        return {
                            storageId: item.split(':')[0],
                            repositoryId: item.split(':')[1],
                            type:"source",
                            policyId: dataForm.policyId,
                        }
                    });
                    data.targetRepositories = this.selectedTargetRepositoriesKeys.map(item => {
                        return {
                            storageId: item.split(':')[0],
                            repositoryId: item.split(':')[1],
                            nodeName: this.federalPromotionPolicyForm.selectTargetNodes.key,
                            nodeType: this.federalPromotionPolicyForm.nodeType === 1 ? "inner" : "JFrog",
                            type:"target",
                            policyId: dataForm.policyId,
                        }
                    });
                    data.pathRules = dataForm.paths.map(item =>{
                        return {
                            attributeValue: item.value,
                            ruleType: "path",
                            policyId: dataForm.policyId,
                        }
                    });
                    data.metadataRules = dataForm.metaDatas.map(item =>{
                        return {
                            attributeKey: item[0].value,
                            attributeValue: item[1].value,
                            ruleType: "metadata",
                            policyId: dataForm.policyId,
                        }
                    });
                    if(dataForm.policyId){
                        data.policyId = dataForm.policyId

                        federalPromotionPolicyEdit(data).then(res => {
                            this.successMsg(this.$t('FederalPromotionPolicy.OperationSuccessful'))
                            this.queryFederalPromotionPolicy();
                            this.federalPromotionPolicyClose();
                        }).catch((err) => {
                            console.log("err",err)
                            this.$notification.error({
                                message: this.$t('FederalPromotionPolicy.OperationFailure'),
                                description: ""
                            })
                        })
                    }else {
                        federalPromotionPolicyAdd(data).then(res => {
                            this.successMsg(this.$t('FederalPromotionPolicy.OperationSuccessful'))
                            this.queryFederalPromotionPolicy();
                            this.federalPromotionPolicyClose();
                        }).catch((err) => {
                            console.log("err",err)
                            this.$notification.error({
                                message: this.$t('FederalPromotionPolicy.OperationFailure'),
                                description: ""
                            })
                        })
                    }

                }
            })
        },

        federalPromotionPolicyClose() {
            this.federalPromotionPolicyResetForm()
            this.visibleDrawer = false
        },

        federalPromotionPolicyResetForm() {

            this.federalPromotionPolicyForm = {
                name: '',
                policyId:'',
                paths: [{
                    value: '',
                    key: Date.now(),
                }],
                metaDatas: [[
                    {
                        value: '',
                        key: Date.now(),
                    },
                    {
                        value: '',
                        key: Date.now(),
                    }
                ]],
                nodeType:1,
                isEnabled: false,
                isDeleteSync: false,
                sourceRepositories: [],
                targetRepositories: [],
            };
            this.federalPromotionPolicyForm.selectTargetNodes = '';
            //this.targetNodesOptions = [];
            console.log("this.targetNodesOptions",this.targetNodesOptions)
            this.selectedSourceRepositoriesKeys = [];
           // this.targetNodesRepositories = [];
            this.selectedTargetRepositories = [];
            this.selectedTargetRepositoriesKeys = [];
            this.repositoriesLoading = false;
            this.repositoriesHasMore = false;
            this.repositoriesTargetLoading = false;
            this.repositoriesTargetHasMore = false;
            //this.repositoriesList = [];
            this.subLayout = undefined;
            this.layout = undefined;
            this.policy = undefined;
            if (this.$refs.federalPromotionPolicyFormRef) {
                this.$refs.federalPromotionPolicyFormRef.resetFields()
            }
        },

        federalPromotionPolicyDelete(item){
            federalPromotionPolicyDelete(item.policyId).then(res => {
                this.successMsg(this.$t('FederalPromotionPolicy.OperationSuccessful'))
                this.queryFederalPromotionPolicy();
            }).catch((err) => {
                console.log("err",err)
                let msg = err.response.data.message ? err.response.data.message : err.response.data.error ? err.response.data.error : err.response.data
                if (msg && msg.length > 0) {
                    this.$notification.error({
                        message: msg,
                        description: ""
                    })
                }
            })
        },

        showFederalPromotionPolicyDatils(item) {
            federalPromotionPolicyDetail(item.policyId).then(res => {
                this.federalPromotionPolicyForm.policyId = res.policyId;
                this.federalPromotionPolicyForm.name = res.name;
                this.federalPromotionPolicyForm.isEnabled = res.isEnabled;
                this.federalPromotionPolicyForm.isDeleteSync = res.isDeleteSync;
                if (res.pathRules && res.pathRules.length > 0) {
                    this.federalPromotionPolicyForm.paths = res.pathRules.map(path => {
                        return {
                            value: path.attributeValue,
                            key: Date.now(),
                        }
                    });
                }
                if (res.metadataRules && res.metadataRules.length > 0) {
                    this.federalPromotionPolicyForm.metaDatas = res.metadataRules.map(metaData => {
                        return [
                            {
                                value: metaData.attributeKey,
                                key: Date.now(),
                            },
                            {
                                value: metaData.attributeValue,
                                key: Date.now(),
                            }
                        ]
                    });
                }
                this.federalPromotionPolicyForm.isEnabled = res.isEnabled;

                if (res.sourceRepositories && res.sourceRepositories.length > 0) {
                    let sourceKey = res.sourceRepositories[0].storageId + ":" + res.sourceRepositories[0].repositoryId;
                    this.subLayout = this.repositoriesList.filter(item => item.key === sourceKey)[0].subLayout;
                    this.policy = this.repositoriesList.filter(item => item.key === sourceKey)[0].policy;
                    this.layout = this.repositoriesList.filter(item => item.key === sourceKey)[0].layout;

                    this.selectedSourceRepositoriesKeys = res.sourceRepositories.map(item => {
                        return item.storageId + ":" + item.repositoryId;
                    });
                }
                if(res.targetRepositories && res.targetRepositories.length > 0){
                    this.federalPromotionPolicyForm.nodeType = res.targetRepositories[0].nodeType === "inner" ? 1 : 2;
                    this.federalPromotionPolicyForm.selectTargetNodes = {
                        key: res.targetRepositories[0].nodeName,
                        value: res.targetRepositories[0].nodeName,
                    }
                    this.handleTargetRepositories(res.targetRepositories[0].nodeName);
                    this.selectedTargetRepositoriesKeys = res.targetRepositories.map(item => {
                        return item.storageId + ":" + item.repositoryId

                    });
                }
                this.visibleDrawer = true;
            }).catch((err) => {
                console.log("err",err)
                this.$notification.error({
                    message: this.$t('FederalPromotionPolicy.OperationFailure'),
                    description: ""
                })
            })

            //this.federalPromotionPolicyForm = res.data.data

        },

        onNodeChange(e){
            this.federalPromotionPolicyForm.selectTargetNodes='';
          //外部节点
          if(e.target.value === 2){
            this.handleExternalNode({type: this.layout})

          } else if(e.target.value === 1) {
            this.targetNodesOptions =[];
            this.selectedTargetRepositories =[];
            if( this.targetNodesRepositories && this.targetNodesRepositories.length > 0){
              this.targetNodesRepositories.forEach(node => {
                let tmp = {
                  key: node.key,
                  label: node.key,
                }
                this.targetNodesOptions.push(tmp);
              })
              this.handleDefNode({ value: this.targetNodesOptions[0].key,
                key: this.targetNodesOptions[0].key})
              this.handleTargetRepositories(this.targetNodesOptions[0].key)
            }
          }
        },

        handleExternalNode(params){
          getExternalNodeRepositories(params).then(res => {
              if (res) {
                this.targetNodesOptions=[];
                this.externalNodeRepositories = [];
                  res.forEach(node => {
                      let json = { key: node.key, artifactoryRepositoryType: '', children: [] }
                      node.repositories.forEach(repo => {
                        json.children.push({ key: repo.key, artifactoryRepositoryType: repo.artifactoryRepositoryType, children: null,name: repo.name, })
                      })
                      let tmp = {
                        key: node.key,
                        label: node.key,
                      }
                      this.targetNodesOptions.push(tmp)
                      this.externalNodeRepositories.push(json)
                  })
                if(this.targetNodesOptions && this.targetNodesOptions.length > 0){
                  this.handleDefNode({ value: this.targetNodesOptions[0].key,
                    key: this.targetNodesOptions[0].key})
                  this.handleSelectedTargetRepositoriesKeys(this.targetNodesOptions[0].key)
                }

              }
          }).finally(() => {
          })
        },

        handleDefNode(data){
          this.federalPromotionPolicyForm.selectTargetNodes =data;
        },

        handleSelectedTargetRepositoriesKeys (key) {
          this.selectedTargetRepositories =[];
            if(this.externalNodeRepositories){
                this.externalNodeRepositories.forEach(node => {
                  if(node.children && node.key === key){
                   node.children.forEach(item => {
                     let temp = {}
                     temp.key = `${node.key}:${item.name}`
                     temp.title = temp.key
                     temp.label = temp.key
                     temp.subLayout = this.subLayout
                     temp.policy = this.policy
                     temp.layout = this.layout
                     this.selectedTargetRepositories.push(temp)
                    });
                  }
              });
            }
        },

        showTargetRepositories(data){
            this.targetRepositoriesList = data;
            this.showTargetRepositoriesModal = true;
        }
    }
}

</script>

<style lang="scss" scoped>
.v-search {
    max-width: 200px;
    width: 170px;
    min-width: 150px;
    margin-left: 5px;
    margin-bottom: 8px;
}

.dynamic-delete-button {
    cursor: pointer;
    position: relative;
    top: 4px;
    font-size: 24px;
    color: #999;
    transition: all 0.3s;
}

.dynamic-add-button {
    cursor: pointer;
    position: relative;
    font-size: 14px;
    color: #3EA1EC;
    transition: all 0.3s;
}

.dynamic-delete-button:hover {
    color: #777;
}

.dynamic-delete-button[disabled] {
    cursor: not-allowed;
    opacity: 0.5;
}

.container {
    display: flex;
    justify-content: end; /* 右对齐 */
    align-items: center;
    padding-top: 6.5px;
}

/* 移除所有表单标签后的冒号 */
/* 移除带有 no-colon 类的表单标签后的冒号 */
.no-colon > label::after {
    content: none;
}
</style>