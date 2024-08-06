<template>
    <a-modal
        v-model="visible"
        :title="$t(`Permissions.${type === 'USER' ? 'SelectUsers' : 'SelectGroups'}`)"
        width="50vw"
        @close="closeModal"
        @ok="handleConfirm"
    >
        <div class="by-flex by-row-right by-m-b-10">
            <a-input-search v-model="searchText" size="small" :placeholder="$t('Groups.EnterTheNameQuery')" @search="handleSearch()" class="by-w-200"/>
        </div>
        <a-table
            :row-selection="{ selectedRowKeys: selectedRowKeys, onChange: onSelectChange }"
            :columns="tableColumns"
            :data-source="tableData"
            size="small"
            :pagination="{ pageSize: limit, current: page, total: total, showLessItems: true }"
            :loading="loading"
            @change="handleChangeTable"
        />
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
            type: '',
            page: 1,
            limit: 10,
            total: 0,
            searchText: '',
            tableData: [],
            selectedRowKeys: [],
        }
    },
    computed: {
        tableColumns() {
            return [ { dataIndex: 'title', title: this.$t('Permissions.SingleName') } ]
        },
    },
    methods: {
        openModal(type = 'USER', selectedRowKeys = [])
        {
            this.tableData = []
            this.type = type
            this.visible = true
            this.selectedRowKeys = selectedRowKeys
            this.page = 1
            this.limit = 10
            this.searchText = ''
            this.querySearch()
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
            const selected = this.tableData.filter(item => {
                return this.selectedRowKeys.includes(item.key)
            })
            this.$emit('confirm', selected, this.type)
            this.closeModal()
        }
    }
}
</script>

<style scoped lang="scss">

</style>