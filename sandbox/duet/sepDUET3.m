function demix = sepDUET(spec,p,q,ker)
 
    NFFT = size(spec.data,1);
    tf1 = [spec.data([2:NFFT - 1],:,1); conj(flipud(spec.data([2:NFFT],:,1)))]; 
    tf2 = [spec.data([2:NFFT - 1],:,2); conj(flipud(spec.data([2:NFFT],:,2)))]; 
        
    R21 = (tf1 + eps) ./ (tf2 + eps);
    
    freq = [(1 : NFFT - 1) ((-(NFFT - 1)) + 1:-1)] * (2 * pi / size(tf1,1));
    fmat = freq(ones(size(tf1,2),1),:)';
    
    size(tf1)
    size(freq)
    size(fmat)
    
    a = abs(R21);
    alpha = a - (1 ./ a);
    
    delta = -imag(log(R21)) ./ fmat;
    
    tfweight = (abs(tf1) .* abs(tf2)) .^ p .* abs(fmat) .^ q;
    
    maxa = 0.7;
    maxd = 3.6;
    abins = 35;
    dbins = 50;
    
    amask = (abs(alpha) < maxa) & (abs(delta) < maxd)
    alpha_vec = alpha(amask);
    delta_vec = delta(amask);
    tfweight = tfweight(amask);
    
    alpha_ind = round(1 + (abins - 1) * (alpha_vec + maxa) / (2 * maxa));
    delta_ind = round(1 + (dbins - 1) * (delta_vec + maxd) / (2 * maxd));
    
    A = full(sparse(alpha_ind,delta_ind,tfweight,abins,dbins));
    smooth2DMat(A,ker);
    
    mesh(linspace(-maxd,maxd,dbins),linspace(-maxa,maxa,abins),A);
    
    work_A = A;
    
    numsources = 2;
    peak_delta = [];
    peak_alpha = [];
    for i = 1 : numsources
        [ind_alpha ind_delta] = find(max(max(work_A)) == work_A);
        
        peak_alpha(i) = ind_alpha;        
        peak_delta(i) = ind_delta;
        
        work_A(ind_alpha,ind_delta) = 0;
    end

    peak_a = (peak_alpha + sqrt(peak_alpha .^ 2 + 4)) /2;
    
    minimum_score = Inf * ones(size(tf1));
    best_ind = zeros(size(tf1));
    for i = 1:length(peak_alpha)
        score = abs((peak_a(i) * exp(-sqrt(-1) * fmat * peak_delta(i)) .* tf1) - tf2) .^ 2 / (1 + peak_a(i)^2);
        mask = (score < minimum_score);
        best_ind(mask) = i;
        minimum_score(mask) = score(mask);
    end
%     mesh(amask)
%    mesh(linspace(-maxd,maxd,dbins),linspace(-maxa,maxa,abins),work_A);   

    data = [];
    for i = 1:numsources
       mask = (best_ind == i);
       data(:,:,i) = ((tf1 + peak_a(i) * exp(sqrt(-1) * fmat * peak_delta(i)) .* tf2) ./ (1 + peak_a(i) ^2)) .* mask;
    end
    
    demix = struct('data',{data},...
                   'begin',{spec.begin},...
                   'end',{spec.end},...
                   'org_ch',{spec.org_ch},...
                   'org_all_ch',{spec.org_all_ch},...
                   'file_names',{spec.file_names},...
                   'window',{spec.window},...
                   'step',{spec.step});    
end
    
    
    
    