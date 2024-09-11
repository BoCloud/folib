<template>
    <a-modal
        v-model="visible"
        :title="$t(`Permissions.${type === 'USER' ? 'SelectUsers' : 'SelectGroups'}`)"
        width="50vw"
    >
        <div class="by-flex by-row-right by-m-b-10">
            <a-input-search v-model="searchText" size="small" :placeholder="$t('Groups.EnterTheNameQuery')" @search="handleSearch()" class="by-w-200"/>
        </div>
        <a-table
            :row-selection="{ selectedRowKeys: selectedRowKeys, onChange: onSelectChange }"
            :columns="tableColumns"
            :data-source="tableData"
            :pagination="{ pageSize: limit, current: page, total: total, showLessItems: true }"
            :loading="loading"
            @change="handleChangeTable"
        />
        <template slot="footer">
            <a-button :style="{ marginRight: '8px' }" @click="closeModal">
                {{ $t(`Permissions.Cancel`) }}
            </a-button>
            <a-button type="primary" :disabled="isStorageAdmin && selectedRowKeys.length > 1" @click="handleConfirm">
                {{ $t(`Permissions.Confirm`) }}
            </a-button>
        </template>
    </a-modal>
</template>

<script>
import { getGroupList } from "@/api/group";
import { queryUser } from "@/api/users";

export default {
    name: "selectUserGroup",
    data()
    {
        return {
            visible: false,
            loading: false,
            isStorageAdmin: false,
            type: '',
            page: 1,
            limit: 10,
            total: 0,
            searchText: '',
            tableData: [],
            pageTableData: {},
            selectedRowKeys: [],
        }
    },
    computed: {
        tableColumns() {
            return [ { dataIndex: 'title', title: this.$t('Permissions.SingleName') } ]
        },
    },
    methods: {
        openModal(type = 'USER', selectedRows = [], isStorageAdmin = false)
        {
            this.tableData = []
            this.pageTableData = selectedRows.length ? { 0 : selectedRows } : {}
            this.type = type
            this.visible = true
            this.selectedRowKeys = selectedRows.map(item => item.key)
            this.page = 1
            this.limit = 10
            this.searchText = ''
            this.querySearch()
            this.isStorageAdmin = isStorageAdmin
        },
        closeModal()
        {
            this.visible = false
        },
        onSelectChange(selectedRowKeys) {
            this.selectedRowKeys = selectedRowKeys;
        },
        handleSearch() {
            this.page = 1
            this.querySearch()
        },
        querySearch()
        {
            this.loading = true
            if (this.type === 'USER')
                this.getUsers()
            else
                this.getGroups()
        },
        getUsers()
        {
            queryUser({username: this.searchText}, {page: this.page, limit: this.limit}).then(res => {
                this.tableData = res.data.rows.map((item, index) => {
                    return {
                        key: item.username,
                        title: item.username,
                    }
                })
                this.pageTableData[this.page] = this.tableData
                this.total = res.data.total
            }).finally(() => {
                this.loading = false
            })
        },
        getGroups()
        {
            getGroupList({ name: this.searchText, page: this.page, limit: this.limit}).then(res => {
                this.tableData = res.data.rows.map(item => {
                    return {
                        key: `${item.id}`,
                        title: item.groupName,
                    }
                })
                this.pageTableData[this.page] = this.tableData
                this.total = res.data.total
            }).finally(() => {
                this.loading = false
            })
        },
        handleChangeTable(pagination) {
            if (pagination) this.page = pagination.current
            this.querySearch()
        },
        handleConfirm()
        {
            const selected = []
            const selectedIds = []
            for (const key in this.pageTableData) {
                const currentData = this.pageTableData[key].filter(item => {
                    return this.selectedRowKeys.includes(item.key) && !selectedIds.includes(item.key)
                })
                selectedIds.push(...currentData.map(item => item.key))
                selected.push(...currentData)
            }
            this.$emit('confirm', selected, this.type)
            this.closeModal()
        }
    }
}
</script>

<style scoped lang="scss">

</style>