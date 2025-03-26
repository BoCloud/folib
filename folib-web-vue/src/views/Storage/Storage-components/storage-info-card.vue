/**
功能：存储信息卡片信息组件
作者：张佳宁
日期：
**/
<template>
    <a-card :bordered="false" id="profile" class="card-profile-head info-card" :bodyStyle="{ padding: 0 }">
        <template #title>
            <a-row type="flex" align="middle">
                <a-col :span="8" class="col-info">
                    <div class="avatar-info" style="margin-top:2px; margin-left: 4px;">
                        <h4 class="font-semibold m-0 by-p-l-4 by-flex">
                            <!--   <a-avatar :title="layoutType === 'isFilter' ? $t('Storage.CreateStorageSpace') :''" :style="layoutType === 'isFilter' ? 'cursor:pointer':''" :size="20" shape="square" @click="createHandleView" src="images/folib/storage.svg" />-->
                            <span class="storage-icon by-flex by-row-center">
                                <svg v-if="currentStorage.storageProvider === 's3'" width="17px" height="12px" viewBox="0 0 17 12" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
                                    <title>形状 2</title>
                                    <g id="页面-1" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                                        <g id="首页备份-20" transform="translate(-272.000000, -259.000000)" fill="#FFFFFF" fill-rule="nonzero">
                                            <g id="编组-5" transform="translate(250.000000, 80.000000)">
                                                <g id="编组-15备份-9" transform="translate(13.000000, 166.000000)">
                                                    <g id="服务项目" transform="translate(8.218884, 9.000000)">
                                                        <g id="云数据库RDS" transform="translate(0.891175, 2.000000)">
                                                            <path d="M12.0929323,14 C9.93446739,14 8.18568683,13.4527903 8.18568683,12.7756039 L8.18568683,11.3075459 C8.18568683,11.9847324 9.93606738,12.5318525 12.0929323,12.5318525 C14.2497972,12.5318525 15.9984889,11.9847324 16,11.3075459 L16,12.7756039 C16,13.4527903 14.2512194,14 12.0929323,14 Z M12.0929323,11.7967494 C9.93446739,11.7967494 8.18568683,11.2495397 8.18568683,10.5723532 L8.18568683,9.10429527 C8.18568683,9.78148176 9.93606738,10.3286019 12.0929323,10.3286019 C14.2497972,10.3286019 15.9984889,9.77987047 16,9.10429527 L16,10.5723532 C16,11.2495397 14.2512194,11.7967494 12.0929323,11.7967494 Z M12.0929323,9.59349872 C9.93446739,9.59349872 8.18568683,9.04637859 8.18568683,8.3691921 L8.18568683,7.39024809 C8.18568683,6.71315111 9.93606738,6.16594146 12.0929323,6.16594146 C14.2497972,6.16594146 15.9984889,6.7147624 16,7.38711504 L16,8.3691921 C16,9.04637859 14.2512194,9.59349872 12.0929323,9.59349872 Z M7.70729214,7.11910494 L7.70729214,8.20546783 C7.70729214,8.21674681 7.70889212,8.226325 7.70889212,8.23760398 L7.70889212,9.05282372 C7.70755516,9.0421676 7.70702031,9.03142493 7.70729214,9.02068758 L7.70729214,10.6494263 C7.70729214,10.662227 7.70889212,10.6718947 7.70889212,10.6830842 L7.70729214,10.6830842 L7.70729214,11.4983039 C7.70597902,11.4876757 7.70544425,11.4769646 7.70569216,11.4662573 L7.70569216,12.3023342 L3.5279608,12.3023342 C1.5663826,12.1949153 0,10.6927519 0,8.86815277 C0,7.32284274 1.1247875,6.01340666 2.66877035,5.58498438 C2.79996889,3.58878362 4.52146087,2.00014798 6.61592649,2.00014798 C8.11156814,1.98719366 9.48633081,2.82563815 10.167887,4.16642862 C11.4062055,4.22184803 12.5564467,4.82714197 13.308741,5.81924704 C12.9072344,5.78075528 12.4832391,5.76034569 12.0417329,5.76034569 C9.64967056,5.76034569 7.70889212,6.36815749 7.70729214,7.11910494 L7.70729214,7.11910494 Z" id="形状"></path>
                                                        </g>
                                                    </g>
                                                </g>
                                            </g>
                                        </g>
                                    </g>
                                </svg>
                                <svg v-else width="15px" height="15px" viewBox="0 0 15 15" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
                                    <title>形状</title>
                                    <g id="页面-1" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                                        <g id="首页备份-20" transform="translate(-273.000000, -212.000000)" fill="#FFFFFF" fill-rule="nonzero">
                                            <g id="编组-5" transform="translate(250.000000, 80.000000)">
                                                <g id="编组-15备份" transform="translate(13.000000, 122.000000)">
                                                    <g id="服务项目" transform="translate(8.605140, 9.000000)">
                                                        <path d="M9.09725285,8.04345703 L9.09725285,4.86923828 C9.09725285,4.16367187 9.29926438,3.52119141 9.72180518,2.95009766 C10.1662306,2.35244141 10.7503806,1.94238281 11.4793055,1.75976562 C12.6459222,1.46923828 13.6896484,1.71660156 14.5768158,2.52509766 C15.071744,2.96835937 15.4016962,3.56269531 15.5178528,4.21181641 C15.7316484,5.37558594 15.4050631,6.37832031 14.5279963,7.19179687 C14.0515858,7.63339844 13.4758529,7.89238281 12.8310994,7.99365234 C12.6459222,8.02353516 12.4590615,8.03847656 12.2705174,8.03847656 C11.2352083,8.04179687 10.2015826,8.04013672 9.16627346,8.04013672 L9.09725285,8.04345703 L9.09725285,8.04345703 Z M8.1461152,8.04345703 L4.94928264,8.04345703 C4.1075679,8.04345703 3.3702258,7.77285156 2.74399003,7.22001953 C2.23727776,6.78007812 1.885441,6.19072266 1.74066607,5.53994141 C1.47805107,4.39111328 1.87702386,3.19414062 2.77934205,2.42382812 C3.26080288,2.01376953 3.8163346,1.75976562 4.44593723,1.67841797 C5.46609548,1.54726562 6.3583131,1.82783203 7.10912265,2.5234375 C7.65287036,3.028125 7.97945568,3.65234375 8.10234603,4.37617187 C8.13096433,4.54052734 8.14443177,4.70654297 8.1461152,4.87255859 C8.14948206,5.91181641 8.14779863,6.94941406 8.14779863,7.98867187 L8.1461152,8.04345703 Z M8.1461228,8.97978516 L8.1461228,12.1390625 C8.14779863,12.9691406 7.87003277,13.6962891 7.30776732,14.3138672 C6.81115563,14.8583984 6.19502044,15.2086914 5.45936177,15.3265625 C4.33314745,15.5075195 3.36517551,15.1837891 2.56554651,14.3852539 C2.13122171,13.9569336 1.83830498,13.4074219 1.72551521,12.8114258 C1.53023739,11.7987305 1.76255065,10.8873047 2.44433959,10.1020508 C2.94600157,9.52431641 3.58907163,9.17900391 4.34661489,9.03789062 C4.55367671,8.99970703 4.76578883,8.98144531 4.97621751,8.98144531 L8.07541116,8.98144531 L8.1461228,8.97978516 L8.1461228,8.97978516 Z M9.09725285,9.01630859 L12.3210203,9.01630859 C12.9741909,9.01630859 13.5802255,9.17900391 14.1290235,9.53095703 C14.8023953,9.96425781 15.2586047,10.5619141 15.4639831,11.3272461 C15.7383821,12.3449219 15.5481546,13.2845703 14.9017177,14.1246094 C14.4572923,14.7006836 13.8697754,15.0791992 13.1610516,15.268457 C12.8108983,15.3647461 12.4455941,15.3962891 12.0836567,15.3614258 C10.8530698,15.2352539 9.96590245,14.6143555 9.4086873,13.5302734 C9.24034435,13.2048828 9.15112259,12.8545898 9.11240371,12.4910156 C9.10061971,12.3764648 9.09725285,12.2619141 9.09725285,12.1457031 C9.09556942,11.1213867 9.09556942,10.0970703 9.09725285,9.07109375 L9.09725285,9.01630859 L9.09725285,9.01630859 Z" id="形状"></path>
                                                    </g>
                                                </g>
                                            </g>
                                        </g>
                                    </g>
                                </svg>
                            </span>
                            <span style="font-size:16px;margin-left: 10px;" v-if="layoutType !== 'isFilter'">{{ currentStorage.id }}
                                <a-icon v-if="hasStoragePermission()" @click="updateHandleView" type="edit" style="color: #008AFF;margin-left: 5px;"/>
                            </span>
                            <div class="by-flex" v-else>
                                <a style="color: #141414;" class="ant-dropdown-link by-m-l-10 by-m-r-10" @click="e => e.preventDefault()">
                                    {{ currentStorage.id }}
                                </a>
                                <a-dropdown overlayClassName="overlayClassName" :trigger="['click']">
                                    <div class="change-store">
                                        <a-icon type="swap" />
                                        {{ $t('Storage.ChangeStorageSpace') }}
                                    </div>
                                    <div slot="overlay" class="overlay-content">
                                        <div class="by-flex by-row-between by-p-l-14 by-p-r-16">
                                            <span class="all-storage">{{ $t('Storage.AllStorageSpace') }}</span>
                                            <a-icon
                                                v-if="$store.state.user.roles.indexOf('ADMIN') > -1"
                                                @click="createHandleView()"
                                                type="plus" class="by-pointer by-font-xs"
                                            />
                                        </div>
                                        <div class="menu-content">
                                            <a-menu @click="handheTableSearch($event,'storageId')" >
                                                <a-menu-item v-for="item in storageData" :key="item.id" :class="{active:currentStorage.id === item.id }" class="by-flex">
                                                    <svg v-if="item.storageProvider === 's3'" width="17px" height="12px" viewBox="0 0 17 12" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
                                                        <title>形状 2</title>
                                                        <g id="页面-1" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                                                            <g id="首页备份-20" transform="translate(-272.000000, -259.000000)" :fill="item.id === currentStorage.id ? '#186DE7' : '#B2B8C3'" fill-rule="nonzero">
                                                                <g id="编组-5" transform="translate(250.000000, 80.000000)">
                                                                    <g id="编组-15备份-9" transform="translate(13.000000, 166.000000)">
                                                                        <g id="服务项目" transform="translate(8.218884, 9.000000)">
                                                                            <g id="云数据库RDS" transform="translate(0.891175, 2.000000)">
                                                                                <path d="M12.0929323,14 C9.93446739,14 8.18568683,13.4527903 8.18568683,12.7756039 L8.18568683,11.3075459 C8.18568683,11.9847324 9.93606738,12.5318525 12.0929323,12.5318525 C14.2497972,12.5318525 15.9984889,11.9847324 16,11.3075459 L16,12.7756039 C16,13.4527903 14.2512194,14 12.0929323,14 Z M12.0929323,11.7967494 C9.93446739,11.7967494 8.18568683,11.2495397 8.18568683,10.5723532 L8.18568683,9.10429527 C8.18568683,9.78148176 9.93606738,10.3286019 12.0929323,10.3286019 C14.2497972,10.3286019 15.9984889,9.77987047 16,9.10429527 L16,10.5723532 C16,11.2495397 14.2512194,11.7967494 12.0929323,11.7967494 Z M12.0929323,9.59349872 C9.93446739,9.59349872 8.18568683,9.04637859 8.18568683,8.3691921 L8.18568683,7.39024809 C8.18568683,6.71315111 9.93606738,6.16594146 12.0929323,6.16594146 C14.2497972,6.16594146 15.9984889,6.7147624 16,7.38711504 L16,8.3691921 C16,9.04637859 14.2512194,9.59349872 12.0929323,9.59349872 Z M7.70729214,7.11910494 L7.70729214,8.20546783 C7.70729214,8.21674681 7.70889212,8.226325 7.70889212,8.23760398 L7.70889212,9.05282372 C7.70755516,9.0421676 7.70702031,9.03142493 7.70729214,9.02068758 L7.70729214,10.6494263 C7.70729214,10.662227 7.70889212,10.6718947 7.70889212,10.6830842 L7.70729214,10.6830842 L7.70729214,11.4983039 C7.70597902,11.4876757 7.70544425,11.4769646 7.70569216,11.4662573 L7.70569216,12.3023342 L3.5279608,12.3023342 C1.5663826,12.1949153 0,10.6927519 0,8.86815277 C0,7.32284274 1.1247875,6.01340666 2.66877035,5.58498438 C2.79996889,3.58878362 4.52146087,2.00014798 6.61592649,2.00014798 C8.11156814,1.98719366 9.48633081,2.82563815 10.167887,4.16642862 C11.4062055,4.22184803 12.5564467,4.82714197 13.308741,5.81924704 C12.9072344,5.78075528 12.4832391,5.76034569 12.0417329,5.76034569 C9.64967056,5.76034569 7.70889212,6.36815749 7.70729214,7.11910494 L7.70729214,7.11910494 Z" id="形状"></path>
                                                                            </g>
                                                                        </g>
                                                                    </g>
                                                                </g>
                                                            </g>
                                                        </g>
                                                    </svg>
                                                    <svg v-else width="15px" height="15px" viewBox="0 0 15 15" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
                                                        <title>形状</title>
                                                        <g id="页面-1" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                                                            <g id="首页备份-20" transform="translate(-273.000000, -212.000000)" :fill="item.id === currentStorage.id ? '#186DE7' : '#B2B8C3'" fill-rule="nonzero">
                                                                <g id="编组-5" transform="translate(250.000000, 80.000000)">
                                                                    <g id="编组-15备份" transform="translate(13.000000, 122.000000)">
                                                                        <g id="服务项目" transform="translate(8.605140, 9.000000)">
                                                                            <path d="M9.09725285,8.04345703 L9.09725285,4.86923828 C9.09725285,4.16367187 9.29926438,3.52119141 9.72180518,2.95009766 C10.1662306,2.35244141 10.7503806,1.94238281 11.4793055,1.75976562 C12.6459222,1.46923828 13.6896484,1.71660156 14.5768158,2.52509766 C15.071744,2.96835937 15.4016962,3.56269531 15.5178528,4.21181641 C15.7316484,5.37558594 15.4050631,6.37832031 14.5279963,7.19179687 C14.0515858,7.63339844 13.4758529,7.89238281 12.8310994,7.99365234 C12.6459222,8.02353516 12.4590615,8.03847656 12.2705174,8.03847656 C11.2352083,8.04179687 10.2015826,8.04013672 9.16627346,8.04013672 L9.09725285,8.04345703 L9.09725285,8.04345703 Z M8.1461152,8.04345703 L4.94928264,8.04345703 C4.1075679,8.04345703 3.3702258,7.77285156 2.74399003,7.22001953 C2.23727776,6.78007812 1.885441,6.19072266 1.74066607,5.53994141 C1.47805107,4.39111328 1.87702386,3.19414062 2.77934205,2.42382812 C3.26080288,2.01376953 3.8163346,1.75976562 4.44593723,1.67841797 C5.46609548,1.54726562 6.3583131,1.82783203 7.10912265,2.5234375 C7.65287036,3.028125 7.97945568,3.65234375 8.10234603,4.37617187 C8.13096433,4.54052734 8.14443177,4.70654297 8.1461152,4.87255859 C8.14948206,5.91181641 8.14779863,6.94941406 8.14779863,7.98867187 L8.1461152,8.04345703 Z M8.1461228,8.97978516 L8.1461228,12.1390625 C8.14779863,12.9691406 7.87003277,13.6962891 7.30776732,14.3138672 C6.81115563,14.8583984 6.19502044,15.2086914 5.45936177,15.3265625 C4.33314745,15.5075195 3.36517551,15.1837891 2.56554651,14.3852539 C2.13122171,13.9569336 1.83830498,13.4074219 1.72551521,12.8114258 C1.53023739,11.7987305 1.76255065,10.8873047 2.44433959,10.1020508 C2.94600157,9.52431641 3.58907163,9.17900391 4.34661489,9.03789062 C4.55367671,8.99970703 4.76578883,8.98144531 4.97621751,8.98144531 L8.07541116,8.98144531 L8.1461228,8.97978516 L8.1461228,8.97978516 Z M9.09725285,9.01630859 L12.3210203,9.01630859 C12.9741909,9.01630859 13.5802255,9.17900391 14.1290235,9.53095703 C14.8023953,9.96425781 15.2586047,10.5619141 15.4639831,11.3272461 C15.7383821,12.3449219 15.5481546,13.2845703 14.9017177,14.1246094 C14.4572923,14.7006836 13.8697754,15.0791992 13.1610516,15.268457 C12.8108983,15.3647461 12.4455941,15.3962891 12.0836567,15.3614258 C10.8530698,15.2352539 9.96590245,14.6143555 9.4086873,13.5302734 C9.24034435,13.2048828 9.15112259,12.8545898 9.11240371,12.4910156 C9.10061971,12.3764648 9.09725285,12.2619141 9.09725285,12.1457031 C9.09556942,11.1213867 9.09556942,10.0970703 9.09725285,9.07109375 L9.09725285,9.01630859 L9.09725285,9.01630859 Z" id="形状"></path>
                                                                        </g>
                                                                    </g>
                                                                </g>
                                                            </g>
                                                        </g>
                                                    </svg>
                                                    <span class="by-m-l-10"> {{ item.id }} </span>
                                                </a-menu-item>
                                            </a-menu>
                                        </div>
                                    </div>
                                </a-dropdown>
                            </div>
                            <!--  <a-tooltip placement="topLeft">
                                <template slot="title">
                                    <span>{{ $t('Storage.s3Storage') }}</span>
                                </template>
                                <a-icon style="margin-left: 15px" v-if="currentStorage.storageProvider === 's3'" type="cloud"
                                    theme="filled" class="text-gray-6 text-lg" />
                            </a-tooltip>-->
                        </h4>
                        <p>
                            <span class="url-value">{{ baseUrl }}api/browse/{{ currentStorage.id }}</span>
                            <a class="copy">
                                <a-tooltip placement="topLeft">
                                    <template slot="title">
                                    <span>{{ $t('Storage.CopyStorageSpacePath') }}</span>
                                    </template>
                                    <a-icon type="copy" @click="copy(baseUrl + 'api/browse/' + currentStorage.id)" />
                                </a-tooltip>
                            </a>
                        </p>
                    </div>
                </a-col>
                <a-col v-if="layoutType === 'isFilter'" :span="16" style="display: flex;align-items: center;justify-content: flex-end">
                    <a-tabs v-if="layoutType === 'isFilter'" class="tabs-sliding" style="margin-top:-2px;margin-right:10px;" default-active-key="1" @change="getTabKey">
                        <a-tab-pane key="1" tab="仓库"></a-tab-pane>
                        <a-tab-pane key="2" tab="概览"></a-tab-pane>
                    </a-tabs>
                    <a-form layout="inline">
                        <a-form-item>
                            <a-input-search
                                class="v-search"
                                style="width:180px;"
                                v-model="queryParams.name"
                                @search="search"
                                :placeholder="$t('Storage.RepositoryNameQuery')"
                            />
                        </a-form-item>
                        <a-form-item>
                            <a-select
                                class="v-search self-icon_search"
                                style="width:180px;"
                                v-model="queryParams.layout"
                                :placeholder="$t('Storage.packageTypeQuery')"
                                @change="search"
                                show-search
                                allowClear
                                option-label-prop="label"
                            >
                                <a-select-option
                                    v-for="(item,index) in typeList"
                                    :key="index"
                                    :value="item.type"
                                    :label="item.name"
                                    v-if="!item.disabled"
                                >
                                    <div class="option_style_item">
                                        <div class="image_item">
                                            <img :src="item.src" style="width: 100%;" alt="">  
                                        </div>
                                        <div>
                                            {{ item.name }}
                                        </div>
                                    </div>
                                </a-select-option>
                            </a-select>
                        </a-form-item>
                        <a-form-item>
                            <a-select 
                                class="v-search"
                                style="width:180px;"
                                v-model="queryParams.type" 
                                show-search
                                allowClear
                                @change="search"
                                option-label-prop="label"
                                :placeholder="$t('Storage.StrategyTypeQuery')"
                            >
                                <a-select-option value="hosted" :label="$t('Storage.Local')">
                                    <div style="width:100%;display: flex;justify-content: space-between;align-items: center;">
                                        <div class="img-back-sty">
                                            <img src="./images/local.svg" style="width: 100%;" alt="">
                                        </div>
                                        {{ $t('Storage.Local') }}
                                    </div>
                                </a-select-option>
                                <a-select-option value="proxy" :label="$t('Storage.Agent')">
                                    <div style="width:100%;display: flex;justify-content: space-between;align-items: center;">
                                        <div class="img-back-sty">
                                            <img src="./images/remote.svg" style="width: 100%;" alt="">
                                        </div>        
                                        {{ $t('Storage.Agent') }}
                                    </div>
                                </a-select-option>
                                <a-select-option value="group" :label="$t('Storage.Combination')">
                                    <div style="width:100%;display: flex;justify-content: space-between;align-items: center;">
                                        <div class="img-back-sty">
                                            <img src="./images/virtual.svg" style="width: 100%;" alt="">
                                        </div>   
                                        {{ $t('Storage.Combination') }}
                                    </div>
                                </a-select-option>
                            </a-select>
                        </a-form-item>
                    </a-form>
                    <!-- <a-button class="repository-setting by-m-l-10" size="small" icon="delete"></a-button>-->
                    <a-button v-if="layoutType === 'isFilter'" class="repository-setting by-m-l-10" size="small" icon="form" @click="updateHandleView"></a-button>
                    <a-button v-if="$store.state.user.roles.indexOf('ADMIN') > -1 && layoutType === 'isFilter'" class="by-m-l-10" size="small" icon="plus"  @click="createHandleView()" >
                    </a-button>
                </a-col>
            </a-row>
        </template>
    </a-card>
</template>

<script>
import { isAdmin } from "@/utils/permission"
import Overview from "../../StorageMonitoring/components/Overview"
import StorageInfo from "../../StorageMonitoring/components/StorageInfo"
import typeList from "./select-type";
export default {
    components:{
        Overview,
        StorageInfo
    },
    props:['currentStorage','baseUrl','layoutType','storageData'],
    data() {
        return {
            queryParams:{
                storageId:'',
                name: undefined,
                layout: undefined,
                type: undefined,
                limit:1000,
                page:1
            },
            typeList,
        }
    },
    computed:{
        
    },
    watch:{
        currentStorage: {
            handler(val){
                if(val){
                    this.queryParams.storageId = val.id
                }
            },
            immediate:true,
            deep:true,
        }
    },
    mounted() {

    },
    methods:{
        emptyQuery(){
            this.queryParams.layout = undefined
            this.queryParams.type = undefined
            this.queryParams.name = undefined
        },
        search(){
            this.handheTableSearch()
        },
        hasStoragePermission() {
            return isAdmin() || this.currentStorage.admin === this.$store.state.user.name
        },
        updateHandleView(){
            this.$emit('updateHandleView')
        },
        copy(val){
            this.$emit('copy',val)
        },
        createHandleView(){
            if(this.$store.state.user.roles.indexOf('ADMIN') > -1){
                this.$emit('createHandleView')
            }
        },
        handheTableSearch(val,type){
            if(type){
                this.$emit('handheTableSearch',val.key, type,this.queryParams)
            }else{
                this.$emit('handheTableSearch',this.queryParams)
            }
        },
        getTabKey(val){
            this.$emit('showOverview',val)
        }
    }
}
</script>
<style lang="scss">
    .overlayClassName{
        .ant-dropdown-menu-vertical{
            max-height:500px;
            overflow: auto;
        }
    }
    .active{
        background: #bae7ff;
        font-weight: 600;
    }
    .ant-dropdown-menu-item:hover{
        background: #bae7ff;
    }
    .option_style_item{
        display:flex;
        justify-content:space-between;
    }
    .image_item{
        border-radius: 4px; 
        width:25px;
        padding: 2px;
        // background-image: linear-gradient(310deg, rgb(250, 251, 252), rgb(221, 221, 221));
        box-shadow: 0px 1px 6px 2px rgba(0, 0, 0, 0.1);
    }
    .ant-dropdown-link{
        font-size: 16px !important;
    }
    .info-card.ant-card{
        padding: 0 !important;
        background: #fff;
        box-shadow: 0px 1px 6px 2px rgba(214, 214, 214, 0.1);
        .ant-card-head-title{
            padding: 0;
        }

        .ant-avatar{
            box-shadow: 0px 0px 6px 1px rgba(0, 0, 0, 0.12);
        }
        .ant-form {
            position: relative;
            top: 2px;
        }
        .ant-form-inline .ant-form-item {
            margin-left: 6px;
            margin-right: 0;
        }
    }

    .add-sty{
        margin-right:20px;
        // box-shadow: 0px 0px 6px 2px rgba(0, 0, 0, 0.1);
        cursor: pointer;
    }
    .img-back-sty{
        width: 25px;
        height: 25px;
        border-radius: 4px;
        padding: 1px;
        background: #fff;
        box-shadow: 0px 0px 6px 2px rgba(0, 0, 0, 0.1);
    }

    .ant-card-head-title {

        .path-label {
            font-family: PingFangSC, PingFang SC;
            font-size: 12px;
            font-weight: 400;
            color: #5A5F80;
        }

        .url-value {
            font-family: PingFangSC, PingFang SC;
            font-size: 12px;
            font-weight: 400;
            color: #186DE7;
            margin-right: 10px;
        }

        .copy {
            position: relative;
            top: 2px;
        }
    }
    .change-store {
        position: relative;
        top: 2px;
        height: 22px;
        background: rgba(24, 144, 255, 0.1);
        border-radius: 4px;
        color: #186DE7;
        font-size: 12px!important;
        font-weight: 400;
        padding: 0 10px;
        cursor: pointer;
    }
    .overlay-content {
        width: 240px;
        background: #fff;
        padding: 15px 0 0 12px;
        box-shadow: 0px 2px 15px -6px rgba(66,73,97,0.9);

        .all-storage {
            font-weight: 600;
            font-size: 16px;
            color: #0B2A42;
            line-height: 24px;
        }
    }
    .menu-content {
        max-height: 450px;
        overflow-y: auto;
        padding-right: 12px;
        margin-top: 10px;

        .ant-menu-vertical {
            border-right: none;

            .ant-menu-item {
                border-radius: 8px;

                &:hover {
                    background: #F4F7FE;
                }
            }
            .active {
                color: #186DE7;
                background: #F4F7FE;
            }
        }
    }
    .storage-icon {
        width: 24px;
        height: 24px;
        border-radius: 5px;
        background: #1c94fc;
        margin-left: 0!important;
    }
</style>
