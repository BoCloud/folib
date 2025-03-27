<template>
    <div>
        <div class="table-operations-container">
          <div class="table-operations">
            <a-row >
              <a-col :span="8">
              <a-button type="primary" @click="handleAdd">
                {{ $t('Setting.AddConfig') }}
              </a-button>
            </a-col>
            <a-col :span="16">
                <a-input-search v-model="dictKey" :placeholder="$t('Setting.SearchName')" @search="handleSearch" allowClear/>
            </a-col>
            </a-row>
          </div>
        </div>
        <a-table :columns="configColumns" :data-source="configList" :row-key="record => record.id" :pagination="pagination">
                <template slot="operation" slot-scope="text, record">
                    <div class="action-btns">
                        <a-button type="link" @click="handleEdit(record)">
                            {{ $t('Setting.Edit') }}
                        </a-button>
                    </div>
                </template>
                <template slot-scope="text" slot="commentslot">
                    {{ text ? text : '--' }}
                 </template>
        </a-table>
        <a-modal
            v-model="modalVisible"
            :title="isAdd?$t('Setting.AddConfig'):$t('Setting.EditConfig')"
            @ok="handleModalOk"
            @cancel="handleModalCancel"
            :confirmLoading="modalLoading"
            :width="450">
             <a-form-model
                ref="formRef"
                :model="form"
                :rules="formRules"
                layout="vertical" :hideRequiredMark="false">
                <a-form-model-item  class="mb-10" :label="$t('Setting.SystemKey')" prop="dictKey">
                    <a-input v-model="form.dictKey" />
                </a-form-model-item>
                <a-form-model-item  class="mb-10" :label="$t('Setting.SystemValue')" prop="dictValue">
                    <a-input v-model="form.dictValue" />
                </a-form-model-item>
                <a-form-model-item  class="mb-10" :label="$t('Setting.SystemComment')" prop="comment">
                    <a-input v-model="form.comment" />
                </a-form-model-item>
            </a-form-model>
        </a-modal>
    </div>
</template>

<script>

import {
  updateSingleDict,
  systemDict
} from "@/api/advanced"

export default {
    name: "systemconfig",
    data() {
        return {
            loading: false,
            name: '',
            configList: [],
            dictKey:undefined,
            modalVisible:false,
            modalLoading:false,
            isAdd:true,
            form : {
                id: undefined,
                dictKey: undefined,
                dictType: 'system_property',
                dictValue: undefined,
                comment:undefined,
                overrideSystemProperty:true  
            },
            formRules:{
                dictKey:[
                    { required: true, message: this.$t('Setting.PleaseEnterDictKey'), trigger: 'change' }
                ],
                dictValue:[
                    { required: true, message: this.$t('Setting.PleaseEnterDictValue'), trigger: 'change' }    
                ]
            },
            configColumns: [
                {
                    title: '属性名',
                    i18nKey: 'Setting.SystemKey',
                    dataIndex: 'dictKey',
                    key: 'dictKey',
                    align: 'center',
                    width: 200, 
                },
                {
                    title: '属性值',
                    i18nKey: 'Setting.SystemValue',
                    dataIndex: 'dictValue',
                    key: 'dictValue',
                    align: 'center',
                    width: 150,
                },
                {
                    title: '描述',
                    i18nKey: 'Setting.SystemComment',
                    dataIndex: 'comment',
                    key: 'comment',
                    align: 'center',
                    width: 200,
                    scopedSlots: { customRender: 'commentslot' },
                },
                {
                    title: '创建时间',
                    i18nKey: 'Groups.CreateBy',
                    dataIndex: 'createTime',
                    key: 'createTime',
                    align: 'center',
                    width: 150, 
                },
                {
                    title: '操作',
                    i18nKey: 'Setting.Operation',
                    dataIndex: 'operation',
                    width: 150,
                    align:'center',
                    scopedSlots: { customRender: 'operation' },
                },
            ],
            pagination: {
                current: 1,
                pageSize: 10,
                total: 0,
                showTotal: total => `${this.$t('Setting.Total')} ${total} ${this.$t('Setting.Items')}`,
                onChange: (page, pageSize) => {
                    this.pagination.current = page;
                    this.pagination.pageSize = pageSize;
                    this.getSystemList(); 
                }
        
            },
        }
    },
    computed: {
        i18nGroupColumns () {
            return this.groupColumns.map(column => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey);
                }
                return column;
            })
        }
    },
    mounted() {
        this.getSystemList();
    },
    methods: {
        getSystemList(){
            let params={}
            params.limit=this.pagination.pageSize;
            params.page=this.pagination.current;
            params.dictKey=this.dictKey
            this.loading=true
            systemDict(params).then(res =>{
                this.pagination.total=res.data.total;
                this.configList=res.data.rows;
                this.loading=false;
            }).catch(error =>{
                this.loading=false;
            })
        },
        handleSearch() {
            this.pagination.current = 1; // 重置到第一页
            this.pagination.pageSize = 10; // 重置每页条数
            this.getSystemList();
        },
        handleAdd(){
            this.isAdd=true;
            this.modalVisible=true;
        },
        handleModalOk(){
            this.$refs.formRef.validate(valid => {
                if (!valid) {
                    this.$message.error(this.$t('Setting.PleaseCompleteRequiredFields'));
                    return;
                }
                this.modalVisible=false;
                this.form.overrideSystemProperty=true;
                updateSingleDict(this.form).then(res=>{
                    this.getSystemList();
                    this.$message.success(this.$t('Setting.OperationSuccessful'))
                    this.$refs.formRef.resetFields();
                }).catch(error => {
                    this.$message.error(error.message || this.$t('Setting.OperationFailed'));
                });
            });
        },
        handleEdit(record){
            this.isAdd=false;
            this.modalVisible=true;
            this.form={...record};
        },
        handleModalCancel(){
            this.modalVisible=false;
            this.$refs.formRef.resetFields();
        }, 
           
    }
}
</script>

<style lang="scss" scoped>
    .table-operations-container {
        display: flex;
        justify-content: flex-end;
        margin-bottom: 16px;
    }
</style>