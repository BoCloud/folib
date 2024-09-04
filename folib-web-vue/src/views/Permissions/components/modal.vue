<template>
    <a-drawer
        placement="right"
        width="65%"
        :title="(isView ? $t('Permissions.View') : isEdit ? $t('Permissions.Edit') : $t('Permissions.Create'))"
        :visible="visible"
        @close="closeModal"
    >
        <a-spin :spinning="spinning">
            <a-form-model ref="form" :model="form" >
                <a-form-model-item
                    prop="name"
                    :rules="[
                        { required: true, message: $t('Permissions.EnterTheNameCreate'), trigger: 'blur' },
                        { pattern: /^[0-9A-Za-z_-]+$/, message: $t('Permissions.EnterTheNamePattern') }
                    ]"
                >
                    <a-input
                        v-model="form.name"
                        :placeholder="$t('Permissions.EnterTheNameCreate')"
                        :disabled="isView || isEdit"
                        :maxLength="100"
                    />
                </a-form-model-item>
            </a-form-model>
            <a-steps v-model="step" size="small" class="step">
                <a-step :title="$t('Permissions.Resources')" :status="step === 0 ? 'process' : 'wait'" :disabled="isAdmin" :description="$t('Permissions.ResourcesDesc')"/>
                <a-step :title="$t('Permissions.Users')" :status="step === 1 ? 'process' : 'wait'" :description="$t('Permissions.UsersDesc')"/>
                <a-step :title="$t('Permissions.Groups')" :status="step === 2 ? 'process' : 'wait'" :description="$t('Permissions.GroupsDesc')"/>
            </a-steps>
            <div v-show="!step">
                <repositories
                    ref="repositories"
                    @getStorageList="getStorageList"
                    @getRepositoriesList="getRepositoriesList"
                    :repositoriesList="repositoriesList"
                    :storageList="storageList"
                    :storageTotal="storageTotal"
                    :repositoriesTotal="repositoriesTotal"
                    :isView="isView || isStorageUser || isStorageAdmin"
                />
            </div>
            <div v-if="step === 1" class="by-flex by-col-stretch">
                <div class="select-content">
                    <div class="title">
                        <span class="by-rela">
                            {{ $t(`Permissions.SelectedUser`) }}
                            <span class="custom-badge">{{userSelectList.length}}</span>
                        </span>
                    </div>
                    <div class="by-flex by-m-t-10 by-m-b-10">
                        <a-input v-model="userSearch" :placeholder="$t('Permissions.Search')" allow-clear class="by-w-300"></a-input>
                        <a-popconfirm
                            :title="$t('Permissions.UserSelected')"
                            :visible="confirmVisible"
                            placement="top"
                            @visibleChange="handleVisibleChange"
                            @confirm="openSelectModal('USER')"
                        >
                            <a-button type="primary" icon="edit" class="by-m-l-10" :disabled="isView"/>
                        </a-popconfirm>
                    </div>
                    <div class="selected-list">
                        <div
                            class="selected-item by-flex by-row-between"
                            v-for="(item, index) in userSelectList"
                            :key="index"
                            :class="{'active': currentUserIndex === item.key}"
                            @click="userClick(item)"
                        >
                            <textOver
                                :text="item.title"
                                :max="20"
                                class="by-m-l-10"
                            />
                            <div class="by-flex">
                                <div v-for="(ele, eIndex) in logoValueMap" :key="eIndex">
                                    <a-avatar
                                        v-if="userAuthMap[item.key] && userAuthMap[item.key].includes(ele.value)"
                                        :size="24"
                                        :src="`images/small-logos/${ele.logo}.svg`"
                                        class="by-m-r-10"
                                    />
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                <div class="by-p-l-32 by-flex-1">
                    <div class="permission-item">
                        <div class="title by-flex by-row-between">
                            <span>{{ $t(`Permissions.SelectedPermissions`) }}</span>
                            <a-button
                                v-if="!isView && userSelectList.length && !isAdmin && !isStorageAdmin"
                                type="primary"
                                size="small"
                                @click="onRepositoriesCheckAllChange"
                            >
                                {{ $t(`Permissions.${repositoriesCheckAll ? 'UnselectAll' : 'CheckAll'}`) }}
                            </a-button>
                        </div>
                        <div>
                            <div v-for="(item, index) in repositoriesOptions" :key="index">
                                <hr class="gradient-line" v-if="index">
                                <a-row type="flex" align="middle">
                                    <a-col>
                                        <a-avatar :size="48" :src="`images/small-logos/${item.logo}.svg`" />
                                    </a-col>
                                    <a-col class="pl-15">
                                        <h6 class="mb-0">{{ item.label }}</h6>
                                        <span class="text-dark ml-1">{{ item.desc }}</span>
                                    </a-col>
                                    <a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
                                        <span class="mr-15">{{ $t(`Permissions.${item.enabled ? 'Enabled' : 'Disabled'}`) }}</span>
                                        <a-switch v-model="item.enabled" :disabled="!userSelectList.length || isView || isAdmin || isStorageAdmin" @change="onRepositoriesChange"/>
                                    </a-col>
                                </a-row>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div v-if="step === 2" class="by-flex by-col-stretch">
                <div class="select-content">
                    <div class="title">
                        <span class="by-rela">
                            {{ $t(`Permissions.SelectedGroups`) }}
                            <span class="custom-badge">{{groupSelectList.length}}</span>
                        </span>
                    </div>
                    <div class="by-flex by-m-t-10 by-m-b-10">
                        <a-input v-model="userSearch" :placeholder="$t('Permissions.Search')" allow-clear class="by-w-300"></a-input>
                        <a-button type="primary" icon="edit" class="by-m-l-10" :disabled="isView || isStorageAdmin" @click="openSelectModal('GROUP')"/>
                    </div>
                    <div class="selected-list">
                        <div
                            class="selected-item by-flex by-row-between"
                            v-for="(item, index) in groupSelectList"
                            :key="index"
                            :class="{'active': currentGroupIndex === item.key}"
                            @click="groupClick(item)"
                        >
                            <textOver
                                :text="item.title"
                                :max="20"
                                class="by-m-l-10"
                            />
                            <div class="by-flex">
                                <div v-for="(ele, eIndex) in logoValueMap" :key="eIndex" >
                                    <a-avatar
                                        v-if="groupAuthMap[item.key] && groupAuthMap[item.key].includes(ele.value)"
                                        :size="24"
                                        :src="`images/small-logos/${ele.logo}.svg`"
                                        class="by-m-r-10"
                                    />
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                <div class="by-p-l-32 by-flex-1">
                    <div class="permission-item">
                        <div class="title by-flex by-row-between">
                            <span>{{ $t(`Permissions.SelectedPermissions`) }}</span>
                            <a-button
                                v-if="!isView && groupSelectList.length && !isAdmin && !isStorageAdmin"
                                type="primary"
                                size="small"
                                @click="onGroupCheckAllChange"
                            >
                                {{ $t(`Permissions.${groupCheckAll ? 'UnselectAll' : 'CheckAll'}`) }}
                            </a-button>
                        </div>
                        <div>
                            <div v-for="(item, index) in repositoriesOptions" :key="index">
                                <hr class="gradient-line" v-if="index">
                                <a-row type="flex" align="middle">
                                    <a-col>
                                        <a-avatar :size="48" :src="`images/small-logos/${item.logo}.svg`" />
                                    </a-col>
                                    <a-col class="pl-15">
                                        <h6 class="mb-0">{{ item.label }}</h6>
                                        <span class="text-dark ml-1">{{ item.desc }}</span>
                                    </a-col>
                                    <a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
                                        <span class="mr-15">{{ $t(`Permissions.${item.enabled ? 'Enabled' : 'Disabled'}`) }}</span>
                                        <a-switch v-model="item.enabled" :disabled="!groupSelectList.length || isView || isAdmin || isStorageAdmin" @change="onGroupChange"/>
                                    </a-col>
                                </a-row>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </a-spin>
        <div class="drawer-footer" v-if="!isView">
            <a-button :style="{ marginRight: '8px' }" @click="closeModal">
                {{ $t(`Permissions.Cancel`) }}
            </a-button>
            <a-button type="primary" :loading="confirmLoading" @click="handleConfirm">
                {{ $t(`Permissions.Confirm`) }}
            </a-button>
        </div>
        <selectUserGroup
            ref="selectUserGroup"
            @confirm="selectUserGroupChange"
        ></selectUserGroup>
    </a-drawer>
</template>

<script>
import repositories from "./repositories.vue";
import selectUserGroup from "./selectUserGroup.vue";
import { getPermissionDetail, createPermission, getPermissionUsers, updatePermission } from "@/api/permissions";
import { queryStorages, queryRepositories } from "@/api/folib";
import { getGroupList } from "@/api/group";
import { uniq } from "lodash/array";
import TextOver from "@/components/Tools/textOver.vue";

export default {
    name: "modal",
    components: {
        TextOver,
        repositories,
        selectUserGroup
    },
    data()
    {
        return {
            visible: false,
            isView: false,
            isEdit: false,
            isAdmin: false,
            isStorageAdmin: false,
            isStorageUser: false,
            confirmVisible: false,
            spinning: false,
            step: 0,
            form: {
                name: '',
            },
            repositoriesList: [],
            storageList: [],
            userSearch: '',
            userSelectList: [],
            userSelectCopyList: [],
            userAuthMap: {},
            currentUserIndex: 0,
            repositoriesCheckedList: [],
            repositoriesCheckAll: false,
            groupSelectList: [],
            groupSelectCopyList: [],
            groupAuthMap: {},
            currentGroupIndex: 0,
            groupCheckedList: [],
            groupCheckAll: false,
            confirmLoading: false,
            repositoriesOptions: [],
            logoValueMap: [],
            repositoriesTotal:0,
            storageTotal:0
        }
    },
    watch: {
        userSearch(val) {
            if (!val) {
                this.userSelectList = this.userSelectCopyList
                return
            }
            this.userSelectList = this.userSelectList.filter(item => {
                return item.title.indexOf(val) !== -1
            })
        },
        step(val) {
            if (val === 1) {
                this.currentUserIndex = this.userSelectList[0]?.key || 0
                this.repositoriesCheckedList = this.userAuthMap[this.currentUserIndex] || []
                this.repositoriesOptions.forEach(item => {
                    item.enabled = this.repositoriesCheckedList.includes(item.value)
                })
            } else if (val === 2) {
                this.currentGroupIndex = this.groupSelectList[0]?.key || 0
                this.groupCheckedList = this.groupAuthMap[this.currentGroupIndex] || []
                this.repositoriesOptions.forEach(item => {
                    item.enabled = this.groupCheckedList.includes(item.value)
                })
            }
        }
    },
    methods: {
        async openModal(id, isView, isAdmin)
        {
            this.initOptions()
            this.visible = true
            this.spinning = true
            this.isView = isView
            this.isAdmin = isAdmin
            this.isStorageAdmin = id?.startsWith('STORAGE_ADMIN_')
            this.isStorageUser = id?.startsWith('STORAGE_USER_')
            this.isEdit = !!id
            this.init()
            await this.getStorageList()
            await this.getRepositoriesList()
            if (id) {
                if (this.isStorageAdmin) this.step = 1
                if (isAdmin) this.step = 1
                this.getDetail(id);
            } else {
                this.spinning = false
            }
        },
        initOptions() {
            this.repositoriesOptions = [
                // {
                //     label: this.$t(`Permissions.Download`),
                //     value: 'ARTIFACTS_RESOLVE',
                //     enabled: false,
                //     logo: 'download',
                //     desc: this.$t(`Permissions.DownloadDesc`)
                // },
                {
                    label: this.$t(`Permissions.DeployCache`),
                    value: 'ARTIFACTS_DEPLOY',
                    enabled: false,
                    logo: 'deployCache',
                    desc: this.$t(`Permissions.DeployCacheDesc`)
                },
                {
                    label: this.$t(`Permissions.DeleteUpdate`),
                    value: 'ARTIFACTS_DELETE',
                    enabled: false,
                    logo: 'deleteUpdate',
                    desc: this.$t(`Permissions.DeleteUpdateDesc`)
                },
                // {
                //     label: this.$t(`Permissions.PromoDistribution`),
                //     value: 'ARTIFACTS_PROMOTION',
                //     enabled: false,
                //     logo: 'promoDistribution',
                //     desc: this.$t(`Permissions.PromoDistributionDesc`)
                // },
            ]
            this.logoValueMap = this.repositoriesOptions.map(item => {
                return {
                    value: item.value,
                    logo: item.logo
                }
            })
        },
        closeModal()
        {
            this.visible = false;
        },
        init() {
            this.form.name = ''
            this.step = 0
            this.userSelectList = []
            this.userSelectCopyList = []
            this.userAuthMap = {}
            this.currentUserIndex = 0
            this.repositoriesCheckedList = []
            this.repositoriesCheckAll = false

            this.groupSelectList = []
            this.groupSelectCopyList = []
            this.groupAuthMap = {}
            this.currentGroupIndex = 0
            this.groupCheckedList = []
            this.groupCheckAll = false
            this.$nextTick(() => {
                this.$refs.repositories.init()
            })
        },
        getDetail(id)
        {
            this.spinning = true;
            getPermissionDetail(id).then(res => {
                const { name, privileges, resources } = res
                this.form.name = name;
                this.userAuthMap = {}
                privileges.users.forEach(item => {
                    this.userAuthMap[item.id] = item.access
                })
                if (privileges.users.length)
                    this.getUsers(Object.keys(this.userAuthMap))

                this.groupAuthMap = {}
                privileges.groups.forEach(item => {
                    this.groupAuthMap[item.id] = item.access
                })
                if (privileges.groups.length)
                    this.getGroups(Object.keys(this.groupAuthMap))

                if (resources[0]?.repositoryId) {
                    this.$refs.repositories.step = 0
                    this.$refs.repositories.radioModel = 'Repositories'
                    const repositoryIds = resources.map(item => `${item.storageId}/${item.repositoryId}`)
                    this.$nextTick(() => {
                        this.$refs.repositories.selectedRowKeys = uniq(repositoryIds)

                        if (resources.some(item => item.path)) {
                            this.$nextTick(() => {
                                this.$refs.repositories.setPath(resources)
                            })
                        }
                    })
                } else {
                    const storageIds = resources.map(item => `${item.storageId}`)
                    this.$refs.repositories.step = 0
                    this.$refs.repositories.radioModel = 'StorageSpace'
                    this.$nextTick(() => {
                        this.$refs.repositories.selectedRowKeys = uniq(storageIds)
                    })
                }
            }).finally(() => {
                this.spinning = false;
            })
        },
        getUsers(userIds)
        {
            getPermissionUsers().then(res => {
                res.users.forEach(item => {
                    if (userIds.includes(item.username)) {
                        this.userSelectList.push({
                            key: item.username,
                            title: item.username,
                        })
                    }
                })
                this.currentUserIndex = this.userSelectList[0].key || 0
                this.repositoriesCheckedList = this.userAuthMap[this.currentUserIndex] || []
                if (this.step === 1) {
                    this.repositoriesOptions.forEach(item => {
                        item.enabled = this.repositoriesCheckedList.includes(item.value)
                    })
                }
            })
        },
        getGroups(groupIds)
        {
            getGroupList({ page: 1, limit: 1000}).then(res => {
                res.data.rows.forEach(item => {
                    if (groupIds.includes(`${item.id}`)) {
                        this.groupSelectList.push({
                            key: `${item.id}`,
                            title: item.groupName,
                        })
                    }
                })
                this.currentGroupIndex = this.groupSelectList[0].key || 0
                this.groupCheckedList = this.groupAuthMap[this.currentGroupIndex] || []
                if (this.step === 2) {
                    this.repositoriesOptions.forEach(item => {
                        item.enabled = this.groupCheckedList.includes(item.value)
                    })
                }
            }).finally(() => {
                this.loading = false
            })
        },
        onRepositoriesChange()
        {
            const checkedValues = this.repositoriesOptions.filter(item => item.enabled).map(item => item.value)
            this.repositoriesCheckedList = checkedValues;
            this.repositoriesCheckAll = checkedValues.length === 4
            this.userAuthMap[this.currentUserIndex] = checkedValues
        },
        onRepositoriesCheckAllChange()
        {
            this.repositoriesCheckAll = !this.repositoriesCheckAll;
            this.repositoriesCheckedList = this.repositoriesCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
            this.userAuthMap[this.currentUserIndex] = this.repositoriesCheckedList
            this.repositoriesOptions.forEach(item => {
                item.enabled = this.repositoriesCheckAll
            })
        },

        onGroupChange()
        {
            const checkedValues = this.repositoriesOptions.filter(item => item.enabled).map(item => item.value)
            this.groupCheckedList = checkedValues;
            this.groupCheckAll = checkedValues.length === 4
            this.groupAuthMap[this.currentGroupIndex] = checkedValues
        },
        onGroupCheckAllChange()
        {
            this.groupCheckAll = !this.groupCheckAll;
            this.groupCheckedList = this.groupCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
            this.groupAuthMap[this.currentGroupIndex] = this.groupCheckedList
            this.repositoriesOptions.forEach(item => {
                item.enabled = this.groupCheckAll
            })
        },
        openSelectModal(type) {
            const selectedRowKeys = type === 'USER' ? this.userSelectList.map(item => item.key) :
            this.groupSelectList.map(item => item.key)
            this.$refs.selectUserGroup.openModal(type, selectedRowKeys, this.isStorageAdmin);
        },
        async getStorageList(page = 1) {
            await new Promise((resolve, reject) => {
                queryStorages({
                  page,
                  limit:20
                }).then(res => {
                    this.storageList = res.data.rows;
                    this.storageTotal = res.data.total
                    resolve()
                }).catch(e => {
                    reject(e)
                })
            })
        },
        async getRepositoriesList(page = 1) {
            await new Promise((resolve, reject) => {
                queryRepositories({
                    page,
                    limit:20
              }).then(res => {
                    this.spinning = false
                    this.repositoriesList = []
                    res.data.rows.forEach(item => {
                        item.key = `${item.storageId}/${item.id}`
                        this.repositoriesList.push({...item})
                    })
                    this.repositoriesTotal = res.data.total
                    resolve()
                }).catch(e => {
                    reject(e)
                    this.spinning = false
                })
            })
        },
        selectUserGroupChange(val, type) {
            if (type === 'USER') {
                this.userSelectList = val
                this.userSelectCopyList = val
                this.currentUserIndex = val[0].key || 0
                const userKeys = val.map(item => `${item.key}`)
                for (const key in this.userAuthMap) {
                    if (!userKeys.includes(key)) this.userAuthMap[key] = []
                }
                this.repositoriesCheckedList = this.userAuthMap[this.currentUserIndex] || []
                this.repositoriesOptions.forEach(item => {
                    item.enabled = this.repositoriesCheckedList.includes(item.value)
                })
            } else {
                this.groupSelectList = val
                this.groupSelectCopyList = val
                this.currentGroupIndex = val[0].key || 0
                const groupKeys = val.map(item => `${item.key}`)
                for (const key in this.groupAuthMap) {
                    if (!groupKeys.includes(key)) this.groupAuthMap[key] = []
                }
                this.groupCheckedList = this.groupAuthMap[this.currentGroupIndex] || []
                this.repositoriesOptions.forEach(item => {
                    item.enabled = this.groupCheckedList.includes(item.value)
                })
            }
        },
        userClick(item) {
            this.currentUserIndex = item.key
            if (this.userAuthMap[item.key]) {
                this.repositoriesCheckedList = this.userAuthMap[item.key]
            } else {
                this.userAuthMap[item.key] = []
                this.repositoriesCheckedList = []
            }
            this.repositoriesCheckAll = this.repositoriesCheckedList.length === 4
            this.repositoriesOptions.forEach(item => {
                item.enabled = this.repositoriesCheckedList.includes(item.value)
            })
        },
        groupClick(item) {
            this.currentGroupIndex = item.key
            if (this.groupAuthMap[item.key]) {
                this.groupCheckedList = this.groupAuthMap[item.key]
            } else {
                this.groupAuthMap[item.key] = []
                this.groupCheckedList = []
            }
            this.groupCheckAll = this.groupCheckedList.length === 4
            this.repositoriesOptions.forEach(item => {
                item.enabled = this.groupCheckedList.includes(item.value)
            })
        },
        handleConfirm() {
            this.$refs.form.validate(validate => {
                if (validate) {
                    const resources = this.$refs.repositories.getResources()
                    if (!resources.length && !this.isAdmin) {
                        this.$message.error(this.$t('Permissions.AtLeastOneRepository'))
                        return
                    }
                    const groups = this.groupSelectList.map(item => {
                        return {
                            id: item.key,
                            access: this.groupAuthMap[item.key] || []
                        }
                    })
                    const users = this.userSelectList.map(item => {
                        return {
                            id: item.key,
                            access: this.userAuthMap[item.key] || []
                        }
                    })
                    if (resources.length) {
                        if (users.some(item => !item.access.length) && !this.isStorageAdmin && !this.isStorageUser) {
                            this.$message.error(this.$t('Permissions.UserNoneSelectAuth'))
                            return
                        }
                        if (groups.some(item => !item.access.length) && !this.isStorageAdmin && !this.isStorageUser) {
                            this.$message.error(this.$t('Permissions.GroupNoneSelectAuth'))
                            return
                        }
                    }
                    this.confirmLoading = true
                    const params = {
                        name: this.form.name,
                        privileges: {
                            groups,
                            users,
                        },
                        resources
                    }
                    const method = this.isEdit ? updatePermission : createPermission;
                    method(params).then(res => {
                        this.visible = false;
                        this.$emit('reset');
                    }).finally(() => {
                        this.confirmLoading = false
                    })
                }
            })
        },
        handleVisibleChange(visible) {
            if (!visible || this.isView) {
                this.confirmVisible = false;
                return;
            }
            if (this.isStorageAdmin && this.userSelectList.length){
                this.confirmVisible = true
            } else {
                this.confirmVisible = false;
                this.openSelectModal('USER')
            }
        },
    }

}
</script>

<style scoped lang="scss">
.step {
    margin-bottom: 20px;

    ::v-deep .ant-steps-item-process .ant-steps-item-icon > .ant-steps-icon{
        color: #FFFFFF;
    }
}

::v-deep .step {
    margin-bottom: 20px;

    .ant-steps-item-process .ant-steps-item-icon > .ant-steps-icon{
        color: #FFFFFF;
    }
}
.resource-item {
    border: 1px solid #c9d0e3;
    border-radius: 4px;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-direction: column;
    padding: 32px;
    cursor: pointer;
    font-size: 14px;
    font-weight: 600;
    span {
        margin-top: 16px;
    }

    &:hover {
        span {
            color: #1890FF;
        }
    }
}

.select-content {
    padding-right: 20px;
    border-right: 1px solid #e9e9e9;

    .selected-list {
        height: calc(100vh - 360px);
        overflow-y: auto;

        .selected-item {
            padding: 8px;
            margin-top: 6px;
            border-radius: 6px;
            cursor: pointer;

            &:hover{
                background: #f1f3f8;
            }
        }

        .active {
            background: rgba(24, 144, 255, 0.1);

            &:hover{
                background: rgba(24, 144, 255, 0.1);
            }
        }
    }
}


.title {
    font-size: 14px;
    font-weight: 600;
    line-height: 1.5;
    margin-bottom: 12px;
}

.permission-item {
    border-bottom: 1px solid #e9e9e9;
    margin-bottom: 24px;
    padding-bottom: 20px;
    &:last-child {
        border-bottom: none;
    }
}

.title .custom-badge {
    top: -2px;
    right: -36px;
}
</style>