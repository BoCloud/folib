<template>
    <div class="groups">
        <a-card :bordered="false" class="header-solid h-full">
            <a-row type="flex" justify="end">
                <a-col :span="2">
                    <a-button type="primary" @click="groupCreate()" style="float: right">
                        {{ $t('Groups.AddGroup') }}
                    </a-button>
                </a-col>
                <a-col :span="3" class="ml-10">
                    <a-input-search v-model="name" :placeholder="$t('Groups.EnterTheNameQuery')" @search="searchGroups()"/>
                </a-col>
            </a-row>
            <a-table
                :columns="i18nGroupColumns"
                :data-source="groupList"
                row-key="id"
                :loading="loading"
                :pagination="{
                    pageSize: limit,
                    current: page,
                    total: total,
                    showLessItems: true,
                    showTotal: total => `${$t('Groups.Total')} ${total} ${$t('Groups.Items')}`
                }"
                @change="handleChangeTable"
            >
                <div slot="groupName" slot-scope="groupName, record">
                    <a-button
                        class="p-0"
                        type="link"
                        size="small"
                        @click="groupCreate(record.id, true)"
                    >{{groupName}}</a-button>
                </div>
                <template slot="joinGroup" slot-scope="joinGroup, record">
                    <div style="display: flex; justify-content: center;">
                        <span>{{ joinGroup === '1' ? $t('Groups.Yes') : $t('Groups.No') }}</span>
                    </div>
                </template>
                <div slot="roles" slot-scope="roles">
                    <div v-if="roles" class="by-flex" style="justify-content: center;">
                        <div
                            v-for="(item, index) in roles.split(',').splice(0, 5)"
                            :key="index"
                            class="custom-tag"
                        >
                            {{ item }}
                        </div>
                        <span class="by-f-w-600">
                            <span v-if="roles.split(',').length > 5">...</span>
                            <!-- <span>({{ roles.split(',').length }})</span>-->
                        </span>
                    </div>
                </div>
                <div slot="operation" slot-scope="text, record">
                    <div class="col-action by-flex-custom">
                        <a-button type="link" size="small" @click="groupCreate(record.id)">
                            <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path class="fill-muted"
                                      d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                                      fill="#111827" />
                                <path class="fill-muted"
                                      d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z" fill="#111827" />
                            </svg>
                        </a-button>
                        <a-popconfirm :title="$t('Setting.SureDelete')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                                      @confirm="handleDelete(record.id)">
                            <a-button type="link" size="small">
                                <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                                    <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                                          d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                                          fill="#111827" />
                                </svg>
                            </a-button>
                        </a-popconfirm>
                    </div>
                </div>
                <template slot="userNumber" slot-scope="userNumber">
                    {{ userNumber || 0 }}
                </template>
            </a-table>
        </a-card>
        <modal ref="modal" @reset="searchGroups()"></modal>
    </div>
</template>

<script>
import { getGroupList, deleteGroup } from "@/api/group";
import modal from './modal.vue'
export default {
    name: "index",
    components: {
        modal
    },
    data() {
        return {
            loading: false,
            name: '',
            limit: 10,
            page: 1,
            total: 0,
            groupList: [],
            groupColumns: [
                {
                    title: '名称',
                    i18nKey: 'Groups.Name',
                    dataIndex: 'groupName',
                    key: 'groupName',
                    width: 200, 
                    scopedSlots: { customRender: 'groupName' },
                },
                {
                    title: '描述',
                    i18nKey: 'Groups.Description',
                    dataIndex: 'description',
                    key: 'description',
                    width: 200,
                    scopedSlots: { customRender: 'description' },
                },
                {
                    title: '用户数',
                    i18nKey: 'Groups.UserNumber',
                    dataIndex: 'userCount',
                    key: 'userCount',
                    width: 100,
                    scopedSlots: { customRender: 'userCount' },
                },
                {
                    title: '创建人',
                    i18nKey: 'Groups.CreateBy',
                    dataIndex: 'createBy',
                    key: 'createBy',
                    width: 150, 
                    scopedSlots: { customRender: 'createBy' },
                },
                // {
                //     title: '权限',
                //     i18nKey: 'Groups.Permissions',
                //     dataIndex: 'roles',
                //     key: 'roles',
                //     width: 400,
                //     align: 'center', 
                //     scopedSlots: { customRender: 'roles' },
                // },
                {
                    title: '自动加入',
                    i18nKey: 'Groups.AutoJoin',
                    dataIndex: 'joinGroup',
                    key: 'joinGroup',
                    width: 100, // 将宽度从 250 减少到 100
                    align:'center',
                    scopedSlots: { customRender: 'joinGroup' },
                },  
                {
                    title: '操作',
                    i18nKey: 'Setting.Operation',
                    dataIndex: 'operation',
                    width: 150,
                    align:'center',
                    scopedSlots: { customRender: 'operation' },
                },
            ]
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
        this.searchGroups()
    },
    methods: {
        groupCreate(id, isView) {
            this.$refs.modal.openModal(id, isView)
        },
        searchGroups() {
            this.page = 1
            this.queryList()
        },
        queryList() {
            this.loading = true
            getGroupList({
                page: this.page,
                limit: this.limit,
                name: this.name
            }).then(res => {
                if (res && res.data) {
                    // 为每个组添加默认为零的计数字段
                    this.groupList = res.data.rows.map(group => ({
                        ...group,
                        userCount: group.userCount || 0  // 如果 userNumber 不存在，则默认为 0
                    }));
                    this.total = res.data.total
                }
            }).finally(() => {
                this.loading = false
            })
        },
        handleChangeTable(pagination) {
            if (pagination) this.page = pagination.current
            this.queryList()
        },
        handleDelete(id) {
            deleteGroup(id).then(res => {
                this.queryList()
            })
        },
        handleJoinGroupChange(checked, record) {
            const newJoinGroup = checked ? '1' : '0';
            record.joinGroup=newJoinGroup;
        //     updateGroup(record.id, { joinGroup: newJoinGroup })
        //         .then(() => {
        //             this.$message.success(this.$t('Groups.UpdateSuccess'));
        //             this.queryList(); // 刷新列表
        //         })
        //         .catch(error => {
        //             this.$message.error(this.$t('Groups.UpdateFailed'));
        //             console.error('Failed to update join group status:', error);
        //         });
        },
    }
}
</script>

<style lang="scss">
.join-status {
    width: 20px;
    height: 20px;
    border-radius: 50%;
    background: rgba(64, 169, 255, 0.2);
    color: #40A9FF;
    display: flex;
    align-items: center;
    justify-content: center;
}

.not-join {
    background: rgba(85, 98, 116, 0.2);
    color: #556274;
}

.by-flex-custom {
    display: flex;
    justify-content: center;
}
</style>