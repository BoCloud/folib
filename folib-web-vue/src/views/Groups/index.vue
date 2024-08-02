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
                    <a-input-search v-model="name" :placeholder="$t('Groups.EnterTheNameQuery')" @search="searchPermissions()"/>
                </a-col>
            </a-row>
            <a-table
                :columns="i18nPermissionColumns"
                :data-source="permissionsList"
                :row-key="(r, i) => i.toString()"
                :pagination="{ pageSize: limit, current: page, total: total, showLessItems: true }"
            >
                <div slot="responseStatus" slot-scope="responseStatus">
                    <a-tag color="#f50" v-if="responseStatus != 200">
                        {{ $t('Setting.Error') }}
                    </a-tag>
                    <a-tag color="#87d068" v-else>
                        {{responseStatus}}
                    </a-tag>
                </div>
                <div slot="operation" slot-scope="text, record">
                    <div class="col-action">
                        <a-popconfirm :title="$t('Setting.SureDelete')" okType="danger" :ok-text="$t('Setting.BeSure')" :cancel-text="$t('Setting.Cancel')"
                                      @confirm="handleDelete(record)">
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
            </a-table>
        </a-card>
        <modal ref="modal" @success="searchPermissions()"></modal>
    </div>
</template>

<script>
import modal from './modal.vue'
export default {
    name: "index",
    components: {
        modal
    },
    data() {
        return {
            name: '',
            limit: 10,
            page: 1,
            total: 0,
            permissionsList: [
                {
                    name: 'test',
                    permissions: 'test',
                    external: 'test',
                    admin: 'test',
                    autoJoin: 'test',
                }
            ],
            permissionsColumns: [
                {
                    title: '名称',
                    i18nKey: 'Groups.Name',
                    dataIndex: 'name',
                    key: 'name',
                    width: 200,
                    scopedSlots: { customRender: 'name' },
                },
                {
                    title: '权限',
                    i18nKey: 'Groups.Permissions',
                    dataIndex: 'permissions',
                    key: 'permissions',
                    scopedSlots: { customRender: 'permissions' },
                },
                {
                    title: '外部',
                    i18nKey: 'Groups.External',
                    dataIndex: 'external',
                    key: 'external',
                    scopedSlots: { customRender: 'external' },
                },
                {
                    title: '管理员',
                    i18nKey: 'Groups.Admin',
                    dataIndex: 'admin',
                    key: 'admin',
                    scopedSlots: { customRender: 'admin' },
                },
                {
                    title: '自动加入',
                    i18nKey: 'Groups.AutoJoin',
                    dataIndex: 'autoJoin',
                    key: 'autoJoin',
                    scopedSlots: { customRender: 'autoJoin' },
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
    methods: {
        groupCreate() {
            this.$refs.modal.openModal()
        },
        searchPermissions() {
            // queryPermissions({name: this.name, page: this.page, limit: this.limit}).then(res => {
            //     this.permissionsList = res.data.data.list;
            //     this.total = res.data.data.total;
            // })
        }
    }
}
</script>

<style scoped lang="scss">

</style>