<template>
    <div class="permission">
        <a-card :bordered="false" class="header-solid h-full">
            <a-row type="flex" justify="end">
                <a-col :span="3" class="ml-10">
                    <a-select v-model="storageId" :placeholder="$t('Permissions.EnterTheStorageQuery')" showSearch
                        allowClear style="width: 100%" @change="getRepositoryList" optionFilterProp="value">
                        <a-select-option v-for="(item, index) in storageList" :key="`${index}`" :value="item.id">
                            {{ item.id }}
                        </a-select-option>
                    </a-select>
                </a-col>
                <a-col :span="3" class="ml-10">
                    <a-select v-model="repositoryId" :placeholder="$t('Permissions.EnterTheRepositoryQuery')" show-search
                        allowClear @change="searchPermissions()" style="width: 100%">
                        <a-select-option v-for="item in repositoryList" :key="item.id" :value="item.id">
                            {{ item.id }}
                        </a-select-option>
                    </a-select>
                </a-col>
                <a-col :span="3" class="ml-10">
                    <a-input-search v-model="path" :placeholder="$t('Permissions.EnterThePathQuery')"
                        @search="searchPermissions()" />
                </a-col>
                <a-col :span="3" class="ml-10">
                    <a-input-search v-model="name" :placeholder="$t('Permissions.EnterTheNameQuery')"
                        @search="searchPermissions()" />
                </a-col>
            </a-row>
            <a-table :columns="i18nPermissionColumns" :data-source="permissionsList" row-key="id" :loading="loading"
                :pagination="{ pageSize: limit, current: page, total: total, showLessItems: true, showTotal: total => `共 ${total} 条` }"
                :scroll="{ x: true }" @change="handleChangeTable">
                <div slot="enName" slot-scope="enName, record">
                  <span>{{ enName }}</span>
                </div>

                <div slot="cnName" slot-scope="cnName">
                    <span>{{ cnName ? cnName : '--' }}</span>
                </div>
                <div slot="users" slot-scope="users">
                    <span>{{ users ? users.split(',').length : 0 }}</span>
                </div>
                <div slot="userGroups" slot-scope="userGroups">
                    <span>{{ userGroups ? userGroups.split(',').length : 0 }}</span>
                </div>
            </a-table>
        </a-card>
    </div>
</template>
<script>
import { getPermissionList, deletePermission } from "@/api/permissions";
import { getStorages, getLibraryFilter, getStoragesAndRepositories} from "@/api/folib";

export default {
    name: "index",
    components: {},
    data() {
        return {
            storageId: undefined,
            storageList: [],
            repositoryList: [],
            repositoryId: undefined,
            path: '',
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
                    width: 300,
                    scopedSlots: { customRender: 'enName' },
                },
                {
                    title: '权限名称',
                    i18nKey: 'Permissions.Description',
                    dataIndex: 'description',
                    key: 'description',
                    width: 400,
                    scopedSlots: { customRender: 'cnName' },
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
                }
            ]
        }
    },
    computed: {
        i18nPermissionColumns() {
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
        this.getStorageList()
    },
    methods: {
        searchPermissions() {
            this.page = 1
            this.queryList()
        },
        handleDelete(data) {
            deletePermission(data).then(res => {
                this.queryList()
            })
        },
        queryList() {
            this.loading = true
            getPermissionList({
                page: this.page,
                limit: this.limit,
                matchRoleName: this.name,
                storageId: this.storageId || '',
                repositoryId: this.repositoryId,
                path: this.path
            }).then(res => {
                if (res && res.data) {
                    const sortedData = res.data.rows.sort((a, b) => {
                        return b.isDefault - a.isDefault;
                    })
                    this.permissionsList = sortedData
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
        getStorageList() {
            getStorages().then(res => {
                this.storageList = res.storages;
            })
        },
        getRepositoryList(val) {
            this.repositoryId = null
            this.repositoryList = []
            this.searchPermissions()
            getLibraryFilter(val).then(res => {
                this.repositoryList = res.repositories
            })
        }
    }
}
</script>

<style lang="scss"></style>
