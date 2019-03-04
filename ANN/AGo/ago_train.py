
def get_lr(step):
    step /= 1000
    if step in range(400):
        return 10e-2
    elif step in range(400, 600):
        return 10e-3
    elif step > 600:
        return 10e-4

def train():
    batch_size = 32

    input_state = torch.randn(batch_size, 17, 19, 19)
    input_state.apply_(lambda x: 0 if x < 0 else 1) 
    target_p = torch.randn(batch_size, 19*19+1)
    target_p = torch.sigmoid(target_p)
    target_v = torch.randn(batch_size)
    target_v = torch.tanh(target_v)

    model = AGoNet()

    mse = nn.MSELoss(reduction='sum')

    step = 0
    optimizer = optim.SGD(model.parameters(), lr=get_lr(step) / 2048 * batch_size,
                          momentum=0.9, weight_decay=10e-4)

    for t in range(1000):
        logit_policy, value = model(input_state)

        policy = torch.sigmoid(logit_policy)
        policy = policy.view(policy.size(0), -1, 1)
        target_p = target_p.view(target_p.size(0), 1, -1)
        loss_p = -torch.bmm(target_p, torch.log(policy))
        loss_p = torch.sum(loss_p)

        loss_v = mse(value, target_v)

        loss = loss_p + loss_v
        print(loss)

        optimizer.zero_grad()
        loss.backward()
        optimizer.step()


