<template>
    <div class="permission">
        <a-card :bordered="false" class="header-solid h-full">
            <a-row type="flex" justify="end">
                <a-col :span="2">
                    <a-button type="primary" @click="userCreate()" style="float: right">
                        {{ $t('Permissions.AddPermission') }}
                    </a-button>
                </a-col>
                <a-col :span="3" class="ml-10">
                    <a-input-search v-model="name" :placeholder="$t('Permissions.EnterTheNameQuery')" @search="searchPermissions()"/>
                </a-col>
            </a-row>
            <a-table
                :columns="i18nPermissionColumns"
                :data-source="permissionsList"
                row-key="id"
                :loading="loading"
                :pagination="{ pageSize: limit, current: page, total: total, showLessItems: true }"
                :scroll="{ x: true }"
                @change="handleChangeTable"
            >
                <div slot="enName" slot-scope="enName, record">
                    <a-button
                        type="link"
                        size="small"
                        :disabled="record.isDefault === '1'"
                        @click="userCreate(record.id, true)"
                    >{{enName}}</a-button>
                </div>
                <div slot="users" slot-scope="users">
                    <div v-if="users" class="by-flex">
                        <div
                            v-for="(item, index) in users.split(',').splice(0, 3)"
                            :key="index"
                            class="custom-tag"
                        >
                            {{ item }}
                        </div>
                        <span class="by-f-w-600">
                            <span v-if="users.split(',').length > 3">...</span>
                            <!-- <span>({{ users.split(',').length }})</span>-->
                        </span>
                    </div>
                </div>
                <div slot="userGroups" slot-scope="userGroups">
                    <div v-if="userGroups" class="by-flex">
                        <div
                            v-for="(item, index) in userGroups.split(',').splice(0, 3)"
                            :key="index"
                            class="custom-tag"
                        >
                            {{ item }}
                        </div>
                        <span class="by-f-w-600">
                            <span v-if="userGroups.split(',').length > 3">...</span>
                            <!--  <span>({{ userGroups.split(',').length }})</span>-->
                        </span>
                    </div>
                </div>
                <div slot="operation" slot-scope="text, record">
                    <div class="col-action by-flex">
                        <a-button type="link" size="small" @click="userCreate(record.id)">
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
                            <a-button v-if="record.isDefault === '0'" type="link" size="small">
                                <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                                    <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                                          d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                                          fill="#111827" />
                                </svg>
                            </a-button>
                        </a-popconfirm>
                    </div>
                </div>
            </a-table>
        </a-card>
        <CreateModal ref="modal" @reset="searchPermissions"></CreateModal>
    </div>
</template>
<script>
import CreateModal from './components/modal.vue'
import { getPermissionList, deletePermission } from "@/api/permissions";

export default {
    name: "index",
    components: {
          CreateModal
    },
    data() {
        return {
            name: '',
            limit: 10,
            page: 1,
            total: 0,
            loading: false,
            permissionsList: [],
            permissionsColumns: [
                {
                    title: '权限名称',
                    i18nKey: 'Permissions.Name',
                    dataIndex: 'enName',
                    key: 'enName',
                    width: 400,
                    scopedSlots: { customRender: 'enName' },
                },
                {
                    title: '用户',
                    i18nKey: 'Permissions.Users',
                    dataIndex: 'users',
                    key: 'users',
                    scopedSlots: { customRender: 'users' },
                },
                {
                    title: '组',
                    i18nKey: 'Permissions.Groups',
                    dataIndex: 'userGroups',
                    key: 'userGroups',
                    scopedSlots: { customRender: 'userGroups' },
                },
                {
                    title: '操作',
                    i18nKey: 'Setting.Operation',
                    dataIndex: 'operation',
                    width: 120,
                    scopedSlots: { customRender: 'operation' },
                },
            ]
        }
    },
    computed: {
        i18nPermissionColumns () {
            return this.permissionsColumns.map(column => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey);
                }
                return column;
            })
        }
    },
    mounted() {
        this.queryList()
    },
    methods: {
        userCreate(id, isView) {
            this.$refs.modal.openModal(id, isView)
        },
        searchPermissions() {
            this.page = 1
            this.queryList()
        },
        handleDelete(id) {
            deletePermission(id).then(res => {
                this.queryList()
            })
        },
        queryList() {
            this.loading = true
            getPermissionList({
                page: this.page,
                limit: this.limit,
                name: this.name
            }).then(res => {
                if (res && res.data) {
                    this.permissionsList = res.data.rows
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
    }
}
</script>

<style lang="scss">

</style>