<template>
    <div>
        <!-- 迁移任务配置页面 -->
        <div v-if="!showMigrateList">
            <div class="table-operations-container">
                <div class="table-operations">
                    <a-button type="primary" @click="handleAdd">
                        {{ $t('Setting.AddTask') }}
                    </a-button>
                </div>
            </div>

            <a-table :columns="taskColumns" :data-source="migrateTasks" :row-key="record => record.id">
                <template slot="action" slot-scope="text, record">
                    <div class="action-btns">
                        <a-button type="link" @click="handleEdit(record)">
                            {{ $t('Setting.Edit') }}
                        </a-button>
                        <a-popconfirm
                            :title="$t('Setting.DeleteConfirm')"
                            :ok-text="$t('Setting.OK')"
                            :cancel-text="$t('Setting.Cancel')"
                            @confirm="deleteTask(record)">
                          <a-button  type="link">
                            {{ $t('Setting.Delete') }}
                          </a-button>
                        </a-popconfirm>
                        <a-button  type="link" @click="handleMigrate(record)">
                            {{ $t('Setting.Migrate') }}
                        </a-button>
                    </div>
                </template>
            </a-table>
        </div>
        
         <!-- 使用新的迁移列表页面组件，传入任务ID -->
         <div v-else>
            <migrate-list-page
                :migrateId="currentTaskId"
                @back="handleBack"
            />
        </div>

        <!-- 编辑/新增弹窗 -->
        <a-modal
            v-model="modalVisible"
            :title="isEdit ? $t('Setting.EditTask') : $t('Setting.AddTask')"
            @ok="handleModalOk"
            @cancel="handleModalCancel"
            :confirmLoading="modalLoading"
            :width="500"
        >
            <a-form-model
                ref="formRef"
                :model="form"
                :rules="formRules"
                layout="vertical"
            >
                <a-form-model-item :label="$t('Setting.BrowsePrefix')" prop="browsePrefix">
                    <a-input v-model="form.browsePrefix" />
                </a-form-model-item>
                <a-form-model-item :label="$t('Setting.ConcurrentRepos')" prop="batchSize">
                    <a-input-number 
                        v-model="form.batchSize"
                        :min="1"
                        style="width: 100%"
                    />
                </a-form-model-item>       
                <a-form-model-item :label="$t('Setting.ThreadsPerRepo')" prop="threadNumber">
                    <a-input-number 
                        v-model="form.threadNumber"
                        :min="1"
                        style="width: 100%"
                    />
                </a-form-model-item>
                <a-form-model-item :label="$t('Setting.RemoteUrl')" prop="remotePreUrl">
                    <a-input v-model="form.remotePreUrl" />
                </a-form-model-item>
                <a-form-model-item :label="$t('Setting.Username')" prop="username">
                    <a-input v-model="form.username" />
                </a-form-model-item>
                <a-form-model-item :label="$t('Setting.Password')" prop="password">
                    <a-input-password v-model="form.password" />
                </a-form-model-item>
                <a-form-model-item :label="$t('Setting.SyncMetadata')" prop="syncMeta">
                    <a-switch 
                        :checked="form.syncMeta === 1"
                        @change="val => form.syncMeta = val ? 1 : 0"
                    />
                </a-form-model-item>
            </a-form-model>
        </a-modal>
    </div>
</template>

<script>
import { task, updateTask, addTask,deleteTask } from '@/api/migrate'
import MigrateListPage from './MigrateListPage.vue'

export default {
    components: {
        MigrateListPage
    },
    data() {
        return {
            showMigrateList: false,
            currentTaskId: null,
            taskColumns:[
                {
                    title: 'ID',
                    dataIndex: 'dictKey',
                    align: 'center',
                    key: 'dictKey',
                },
                {
                    title: this.$t('Setting.CreateTime'),
                    dataIndex: 'viewTime',
                    align: 'center',
                    key: 'viewTime',
                },
                {   
                    title: this.$t('Setting.Operator'),
                    dataIndex: 'dictValue',
                    align: 'center',
                    key: 'dictValue',
                },
                {
                    title: this.$t('Setting.RepositoryCount'),
                    dataIndex: 'total',
                    align: 'center',
                    key: 'total',
                },
                {
                    title: this.$t('Setting.BrowsePrefix'),
                    dataIndex: 'browsePrefix',
                    align: 'center',
                    key: 'browsePrefix',
                    scopedSlots: { customRender: 'browsePrefix' },
                },
                {
                    title: this.$t('Setting.ConcurrentRepos'),
                    dataIndex: 'batchSize',
                    align: 'center',
                    key: 'batchSize',
                    scopedSlots: { customRender: 'batchSize' },
                },
                {
                    title: this.$t('Setting.ThreadsPerRepo'),
                    dataIndex: 'threadNumber',
                    align: 'center',
                    key: 'threadNumber',
                    scopedSlots: { customRender: 'threadNumber' },
                },
                // {
                //     title: this.$t('Setting.Status'),
                //     dataIndex: 'status',
                //     align: 'center',
                //     key: 'status',
                // },
                {
                    title: this.$t('Setting.Actions'),
                    dataIndex: 'action',
                    key: 'action',
                    align: 'center',
                    scopedSlots: { customRender: 'action' },
                },
            ],
            migrateTasks:[],
            editingKey: '',
            migrateData: [],
            tableLoading: false,
            pagination: {
                current: 1,
                pageSize: 10,
                total: 0,
                showTotal: total => `${this.$t('Setting.Total')} ${total} ${this.$t('Setting.Items')}`
            },
            isEdit: false,
            modalVisible: false,
            modalLoading: false,
            form: {
                id: null,
                browsePrefix: '',
                batchSize: 1,
                threadNumber: 4,
                remotePreUrl: '',
                username: '',
                password: '',
                syncMeta: 1
            },
            formRules: {
                browsePrefix: [
                    { required: true, validator: this.validateUrl, trigger: 'blur' }
                ],
                batchSize: [
                    { required: true, message: this.$t('Setting.PleaseInputConcurrentRepos'), trigger: 'change' }
                ],
                threadNumber: [
                    { required: true, message: this.$t('Setting.PleaseInputThreadsPerRepo'), trigger: 'change' }
                ],
                remotePreUrl: [
                    { required: true, message: this.$t('Setting.PleaseInputRemoteUrl'), trigger: 'blur' },
                    { validator: this.validateUrl, trigger: 'blur' }
                ],
                username: [
                    { required: false, message: this.$t('Setting.PleaseInputUsername'), trigger: 'blur' }
                ],
                password: [
                    { required: false, message: this.$t('Setting.PleaseInputPassword'), trigger: 'blur' }
                ]
            },
            labelCol: { span: 6 },
            wrapperCol: { span: 16 },
        }
    },

    mounted() {
        this.getTask();
    },

    methods: {
        getTask(){
            task().then(res => {
                this.migrateTasks = res.map(item => {
                    const values=JSON.parse(item.alias);
                    const {alias,...rest}=item;
                    const date = new Date(item.createTime);
                    const year = date.getFullYear();
                    const month = String(date.getMonth() + 1).padStart(2, '0');
                    const day = String(date.getDate()).padStart(2, '0');
                    rest.viewTime = `${year}-${month}-${day}`;
                    return {...rest,...values};
                })
            })
        },
        handleEdit(record) {
            this.isEdit = true;
            this.form = {
                id: record.id,
                total: record.total,
                status: record.status,
                migrateId: record.dictKey,
                browsePrefix: record.browsePrefix,
                batchSize: record.batchSize,
                threadNumber: record.threadNumber,
                remotePreUrl: record.remotePreUrl,
                username: record.username,
                password: record.password,
                syncMeta: record.syncMeta ?? 1
            };
            this.modalVisible = true;
        },
        handleModalCancel() {
            this.modalVisible = false;
            this.$refs.formRef.resetFields();
        },
        async handleModalOk() {
            try {
                await this.$refs.formRef.validate();
                this.modalLoading = true;

                if (this.isEdit) {
                    const id = this.form.id;
                    this.form.id = null;
                    await updateTask(id, this.form);
                } else {
                    await addTask(this.form);
                }

                this.$notification.success({
                    message: this.$t('Setting.Success'),
                    description: this.$t(this.isEdit ? 'Setting.UpdateSuccess' : 'Setting.AddSuccess')
                });

                // 刷新列表
                await this.getTask();

                this.modalVisible = false;
                this.$refs.formRef.resetFields();
            } catch (error) {
                this.$notification.error({
                    message: this.$t('Setting.Error'),
                    description: error.message
                });
            } finally {
                this.modalLoading = false;
            }
        },
        validateUrl(rule, value, callback) {
            if (!value) {
                callback();
                return;
            }
            try {
                new URL(value);
                callback();
            } catch (e) {
                callback(new Error(this.$t('Setting.InvalidUrl')));
            }
        },
        handleMigrate(record) {
            this.currentTaskId = record.dictKey;
            this.showMigrateList = true;
        },
        handleBack() {
            this.showMigrateList = false;
            this.currentTaskId = null;
            this.getTask();
        },
        handleAdd() {
            this.isEdit = false;
            this.form = {
                id: null,
                browsePrefix: '',
                batchSize: 1,
                threadNumber: 1,
                remotePreUrl: '',
                username: '',
                password: '',
                syncMeta: 1
            };
            this.modalVisible = true;
        },
        deleteTask(record){
          deleteTask(record.id).then(()=>{
            this.$notification.success({
              message: this.$t('Setting.Success'),
              description: this.$t('Setting.DeleteSuccess')
            });
            this.getTask();
          }).catch(error=>{
            this.$notification.error({
              message: this.$t('Setting.Error'),
              description: error.response.data.error
            });
          })
        }
    },
}
</script>

<style lang="scss" scoped>
.table-operations-container {
    display: flex;
    justify-content: flex-end;
    margin-bottom: 16px;
}

.action-btns .ant-btn {
    padding: 0 4px;
}

::v-deep .ant-form-item {
    margin-bottom: 8px;
    
    .ant-form-item-label {
        padding-bottom: 4px;
        
        label {
            height: 24px;
            line-height: 24px;
        }
    }

    .ant-form-item-control {
        line-height: 32px;
    }
}

::v-deep .ant-modal-body {
    padding: 16px 24px 0;
}

::v-deep .ant-modal-footer {
    padding-top: 16px;
}
</style>
  