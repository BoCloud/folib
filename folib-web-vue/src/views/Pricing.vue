<!-- 
	This is the Pricing page, it uses the dashboard layout in: 
	"./layouts/Default.vue" .
 -->

<template>
	<div>

		<!-- Pricing Headings -->
		<div class="pricing-header" style="background: linear-gradient( rgba(0, 0, 0, 0.25), rgba(0, 0, 0, 0.25) ), url('images/bg-signup.jpg')">
			<div class="content">
				<h3 class="mb-5">See our pricing</h3>
				<p class="text-lg">You have Free Unlimited Updates and Premium Support on each package.</p>
			</div>
		</div>
		<!-- / Pricing Headings -->

		
		<div class="pricing-wrapper">

			<!-- Subscription Period Slider -->
			<div class="sliding-tab">
				<ul>
					<li class="tab"><a href="#" @click.prevent="subscriptionChange('monthly', $event)">Monthly</a></li>
					<li class="tab"><a href="#" @click.prevent="subscriptionChange('annual', $event)">Annual</a></li>
				</ul>
				<div ref="tabBg" class="tab-bg"></div>
			</div>
			<!-- Subscription Period Slider -->

			<!-- Main Container -->
			<div class="container">
				<a-row type="flex" :gutter="[24,24]">
					<a-col :span="24" :lg="8">
						
						<!-- Pricing card -->
						<a-card class="card-pricing header-solid" :bodyStyle="{padding: '20px'}">
							<a-tag class="bg-gray-4 text-gray-9 font-bold">
								STARTER
							</a-tag>
							<h1 class="font-semibold">
								<small>$</small>
								<span v-if="subscription == 'annual'">119</span>
								<span v-else>59</span>
							</h1>

							<ul class="list-pricing">
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									2 team members
								</li>
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									20GB Cloud storage
								</li>
								<li>
									<a-avatar :size="24">
										<a-icon type="minus" />
									</a-avatar>
									Integration help
								</li>
								<li>
									<a-avatar :size="24">
										<a-icon type="minus" />
									</a-avatar>
									Sketch Files
								</li>
								<li>
									<a-avatar :size="24">
										<a-icon type="minus" />
									</a-avatar>
									API Access
								</li>
								<li>
									<a-avatar :size="24">
										<a-icon type="minus" />
									</a-avatar>
									Complete documentation
								</li>
							</ul>
							
							<a-button type="dark" block>
								JOIN <a-icon type="arrow-right" class="ml-5" />
							</a-button>
						</a-card>
						<!-- / Pricing card -->
						
					</a-col>
					<a-col :span="24" :lg="8">
						
						<!-- Pricing card -->
						<a-card class="card-pricing header-solid" :bodyStyle="{padding: '20px'}">
							<a-tag class="bg-gray-4 text-gray-9 font-bold">
								PREMIUM
							</a-tag>
							<h1 class="font-semibold">
								<small>$</small>
								<span v-if="subscription == 'annual'">159</span>
								<span v-else>89</span>
							</h1>

							<ul class="list-pricing">
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									10 team members
								</li>
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									40GB Cloud storage
								</li>
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									Integration help
								</li>
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									Sketch Files
								</li>
								<li>
									<a-avatar :size="24">
										<a-icon type="minus" />
									</a-avatar>
									API Access
								</li>
								<li>
									<a-avatar :size="24">
										<a-icon type="minus" />
									</a-avatar>
									Complete documentation
								</li>
							</ul>
							
							<a-button type="primary" block>
								TRY PREMIUM <a-icon type="arrow-right" class="ml-5" />
							</a-button>
						</a-card>
						<!-- / Pricing card -->
						
					</a-col>
					<a-col :span="24" :lg="8">
						
						<!-- Pricing card -->
						<a-card class="card-pricing header-solid" :bodyStyle="{padding: '20px'}">
							<a-tag class="bg-gray-4 text-gray-9 font-bold">
								ENTERPRISE
							</a-tag>
							<h1 class="font-semibold">
								<small>$</small>
								<span v-if="subscription == 'annual'">399</span>
								<span v-else>99</span>
							</h1>

							<ul class="list-pricing">
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									Unlimited team members
								</li>
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									100GB Cloud storage
								</li>
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									Integration help
								</li>
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									Sketch Files
								</li>
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									API Access
								</li>
								<li>
									<a-avatar :size="24" class="bg-dark">
										<a-icon type="check" />
									</a-avatar>
									Complete documentation
								</li>
							</ul>
							
							<a-button type="dark" block>
								JOIN <a-icon type="arrow-right" class="ml-5" />
							</a-button>
						</a-card>
						<!-- / Pricing card -->
						
					</a-col>
				</a-row>

				<h6 class="text-center text-gray-7 font-semibold">More than 50+ brands trust Soft</h6>

				<!-- Clients -->
				<a-row class="row-clients mt-20">
					<a-col :span="12" :md="8" :lg="4">
						<img src="images/logos/gray-logos/logo-coinbase.svg" alt="logo_coinbase">
					</a-col>
					<a-col :span="12" :md="8" :lg="4">
        				<img src="images/logos/gray-logos/logo-nasa.svg" alt="logo_nasa">
					</a-col>
					<a-col :span="12" :md="8" :lg="4">
        				<img src="images/logos/gray-logos/logo-netflix.svg" alt="logo_netflix">
					</a-col>
					<a-col :span="12" :md="8" :lg="4">
        				<img src="images/logos/gray-logos/logo-pinterest.svg" alt="logo_pinterest">
					</a-col>
					<a-col :span="12" :md="8" :lg="4">
        				<img src="images/logos/gray-logos/logo-spotify.svg" alt="logo_spotify">
					</a-col>
					<a-col :span="12" :md="8" :lg="4">
        				<img src="images/logos/gray-logos/logo-vodafone.svg" alt="logo_vodafone">
					</a-col>
				</a-row>
				<!-- / Clients -->

				<!-- FAQ -->
				<a-row type="flex">
					<a-col :span="24" :md="12" class="mx-auto">
						<h2 class="text-center">Frequently Asked Questions</h2>
						<p class="text-center text-lg">A lot of people don't appreciate the moment until it’s passed. I'm not trying my hardest, and I'm not trying to do</p>
					</a-col>
				</a-row>

				<a-row type="flex">
					<a-col :span="24" :md="20" class="mx-auto">
				
						<a-collapse v-model="faq" :bordered="false" accordion>
							<a-collapse-panel key="1" header="How do I order?" :showArrow="false">
								<p>
									We’re not always in the position that we want to be at. We’re constantly growing.
									We’re constantly making mistakes. We’re constantly trying to express ourselves and
									actualize our dreams. If you have the opportunity to play this game of life you
									need to appreciate every moment. A lot of people don’t appreciate the moment until
									it’s passed.
								</p>
								<a-icon slot="extra" :type="faq == 1 ? 'minus' : 'plus'" />
							</a-collapse-panel>
							<a-collapse-panel key="2" header="How can i make the payment?" :showArrow="false">
								<p>
									It really matters and then like it really doesn’t matter. What matters is the people
									who are sparked by it. And the people who are like offended by it, it doesn’t matter.
									 Because it's about motivating the doers. Because I’m here to follow my dreams and
									 inspire other people to follow their dreams, too.
									We’re not always in the position that we want to be at. We’re constantly growing. We’re
									constantly making mistakes. We’re constantly trying to express ourselves and actualize
									our dreams. If you have the opportunity to play this game of life you need to appreciate
									every moment. A lot of people don’t appreciate the moment until it’s passed.
								</p>
								<a-icon slot="extra" :type="faq == 2 ? 'minus' : 'plus'" />
							</a-collapse-panel>
							<a-collapse-panel key="3" header="How much time does it take to receive the order?" :showArrow="false">
								<p>
									The time is now for it to be okay to be great. People in this world shun people for
									being great. For being a bright color. For standing out. But the time is now to be
									okay to be the greatest you. Would you believe in what you believe in, if you were 
									the only one who believed it? If everything I did failed - which it doesn't, it 
									actually succeeds - just the fact that I'm willing to fail is an inspiration. People 
									are so scared to lose that they don't even try. Like, one thing people can't say is 
									that I'm not trying, and I'm not trying my hardest, and I'm not trying to do the best 
									way I know how.
								</p>
								<a-icon slot="extra" :type="faq == 3 ? 'minus' : 'plus'" />
							</a-collapse-panel>
							<a-collapse-panel key="4" header="Can I resell the products?" :showArrow="false">
								<p>
									I always felt like I could do anything. That’s the main thing people are controlled by!
									Thoughts- their perception of themselves! They're slowed down by their perception of 
									themselves. If you're taught you can’t do anything, you won’t do anything. I was taught 
									I could do everything.
									If everything I did failed - which it doesn't, it actually succeeds - just the fact that 
									I'm willing to fail is an inspiration. People are so scared to lose that they don't even 
									try. Like, one thing people can't say is that I'm not trying, and I'm not trying my 
									hardest, and I'm not trying to do the best way I know how.
								</p>
								<a-icon slot="extra" :type="faq == 4 ? 'minus' : 'plus'" />
							</a-collapse-panel>
							<a-collapse-panel key="5" header="Where do I find the shipping details?">
								<p>
									There’s nothing I really wanted to do in life that I wasn’t able to get good at. That’s 
									my skill. I’m not really specifically talented at anything except for the ability to learn. 
									That’s what I do. That’s what I’m here for. Don’t be afraid to be wrong because you can’t 
									learn anything from a compliment. I always felt like I could do anything. That’s the main 
									thing people are controlled by! Thoughts- their perception of themselves! They're slowed 
									down by their perception of themselves. If you're taught you can’t do anything, you won’t 
									do anything. I was taught I could do everything.
								</p>
								<a-icon slot="extra" :type="faq == 5 ? 'minus' : 'plus'" />
							</a-collapse-panel>
						</a-collapse>
				
					</a-col>
				</a-row>
				<!-- / FAQ -->

			</div>
			<!-- / Main Container -->


		</div>

	</div>
</template>

<script>

	export default ({
		data() {
			return {

				// Subscription type
				subscription: 'monthly',

				// Active faq card
				faq: 1,

			}
		},
		methods: {

			// Handle subscription change and slides
			subscriptionChange( subscription, event ) {

				if( subscription == this.subscription ){
					return ;
				}

				this.subscription = subscription ;

				const tabBg = this.$refs.tabBg,
					  target = event.target ;
				
            	tabBg.style.width = target.offsetWidth + 'px';
            	tabBg.style.height = target.offsetHeight + 'px';
            	tabBg.style.transform = 'translate3d(' + target.offsetLeft + 'px, 0px, 0px)';
			},
			
		},
	})

</script>

<style lang="scss">
</style>